;;;; repair.lisp --- Recover from damage in log files.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; Reconstructing indices

(defun reconstruct-indices (stream chunks)
  "Reconstruct index information for CHUNKS based on the contents of
   STREAM. CHUNKS is a list of entries of the form

     (ID . OFFSET)

   . Return a list of corresponding `indx' instances."
  (log:info "~@<Reconstructing indices for ~:D chunk~:P in ~A~@:>"
            (length chunks) stream)
  (let+ ((indices (make-hash-table))
         ((&flet add-to-index (id entry)
            (push entry (gethash id indices))))
         ((&flet+ make-index ((id . entries))
            "Make and return an `indx' instance for the channel and
             entries ID-AND-ENTRIES."
            (make-instance
             'indx
             :channel-id id
             :count      (length entries)
             :entries    (coerce (nreverse entries) 'vector)))))
    ;; For each chunk, visit all its entries and add them to the
    ;; temporary index lists.
    (iter (for (id . offset) each chunks :with-index i)
          (restart-case
              (let+ ((chunk (unpack stream :block offset))
                     ((&accessors-r/o (chunk-id chnk-chunk-id)) chunk))
                (iter (for entry each (chnk-entries chunk) :with-index j)
                      (with offset1 = 0)
                      (let+ (((&accessors-r/o
                               (channed-id chunk-entry-channel-id)
                               (timestamp  chunk-entry-timestamp)
                               (size       chunk-entry-size)) entry))
                        (add-to-index channed-id
                                      (make-instance 'index-entry
                                                     :timestamp timestamp
                                                     :chunk-id  chunk-id
                                                     :offset    offset1))
                        (incf offset1 (+ 16 size)))))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Ignore remaining chunks.~@:>"))
              (declare (ignore condition))
              (return)))
          #+sbcl (sb-ext:gc :full t))
    ;; Convert temporary index lists into `indx' instances.
    (mapcar #'make-index (hash-table-alist indices))))

;;; Finding undamaged blocks

(defun find-next-block (stream
                        &key
                        (blocks (hash-table-alist *byte-pattern->block-class*)))
  "Return as two values the file position and class name of the next
   block in STREAM.

   The file position of STREAM is changed. When a block is found, the
   file position of STREAM is changed to the start of that block.

   The searched-for block classes can be controlled via BLOCKS. When
   supplied, BLOCKS has to be an alist with elements of the form

     (PATTERN . BLOCK-CLASS)

   where PATTERN is a block tag as a `nibbles:octet-vector' and
   BLOCK-CLASS is a block class."
  (let* ((buffer-length (* 1024 1024))
         (buffer        (nibbles:make-octet-vector buffer-length))
         (eof?          (not (listen stream))))
    (declare (dynamic-extent buffer))
    (iter (until eof?)
          (for global-offset next (file-position stream))
          (let ((read (read-sequence buffer stream)))
            ;; Search for tags of all known block classes in BUFFER.
            (iter (for ((the nibbles:simple-octet-vector pattern) . class) in blocks)
                  (when-let* ((local-offset (search pattern buffer :end2 read))
                              (offset       (+ global-offset local-offset)))
                    (file-position stream offset)
                    (return-from find-next-block
                      (values offset (class-name class)))))
            (setf eof? (not (listen stream)))
            ;; Back up to catch instances spanning buffer borders.
            (when (> (+ global-offset read) 4)
              (file-position stream (+ global-offset read -4))))))
  (values))
