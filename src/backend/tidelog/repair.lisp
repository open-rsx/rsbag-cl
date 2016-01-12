;;;; repair.lisp --- Recover from damage in log files.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; Reconstructing indices

(defun reconstruct-indices (stream chunks ensure-index)
  "Reconstruct index information for CHUNKS based on the contents of
   STREAM. CHUNKS is a list of entries of the form

     (ID . OFFSET)

   . Call ENSURE-INDEX with a channel number to retrieve (creating it
   if necessary) an `index' instance and add entries. Returns
   nothing."
  (log:info "~@<Reconstructing indices for ~:D chunk~:P in ~A~@:>"
            (length chunks) stream)
  ;; For each channel there is a queue of `index-entry' instances to
  ;; that should eventually be added to the corresponding index.
  (let+ ((queues (make-hash-table))
         ((&flet make-queue ()
            (make-array 0 :adjustable t :fill-pointer 0)))
         ((&flet flush-queue (queue)
            (fill queue nil)
            (setf (fill-pointer queue) 0)))
         ((&flet add-to-queue (channel-id entry)
            (vector-push-extend
             entry (ensure-gethash channel-id queues (make-queue)))))
         ((&flet flush-indices ()
            (log:info "~@<Flushing ~:D collected index entr~:@P~@:>"
                      (reduce #'+ (hash-table-values queues) :key #'length))
            (iter (for (channel-id queue) :in-hashtable queues)
                  (let ((index (funcall ensure-index channel-id)))
                    (index-add-entries index queue chunks))
                  (flush-queue queue)))))
    ;; For each chunk, visit all its entries and add them to the
    ;; temporary index lists.
    (rsbag:function-calling-restart-bind
        (((continue (&optional condition) skip bail)
          :report (lambda (stream1)
                    (format stream1
                            (if skip
                                "~@<Skip ahead to the next undamaged ~
                                 block in ~A.~@:>"
                                "~@<Do not process the remainder of ~
                                 ~A.~@:>")
                            stream)))
         ((abort (&optional condition) bail)
          :report (lambda (stream1)
                    (format stream1 "~@<Do not process the remainder ~
                                     of ~A.~@:>"
                            stream))))
      (iter (when (first-iteration-p)
              (setf skip (lambda (&optional condition)
                           (declare (ignore condition))
                           (next-iteration))
                    bail (lambda (&optional condition)
                           (declare (ignore condition))
                           (return))))
            (for (id . offset) each chunks :with-index i)
            ;; Flush collected index entries into the respective
            ;; `index' instances where they will be integrated into
            ;; a memory-efficient representation. This limits the
            ;; peak memory use.
            (with offset/previous = 0)
            (when (> (- offset offset/previous) (ash 1 27))
              (flush-indices)
              #+sbcl (sb-ext:gc :full t)
              (setf offset/previous offset))
            ;; Make and collect index entries for chunk entries.
            (let+ ((chunk (unpack stream :block offset))
                   ((&accessors-r/o (chunk-id chnk-chunk-id)) chunk))
              (iter (for entry each (chnk-entries chunk) :with-index j)
                    (with offset1 = 0)
                    (let+ (((&structure-r/o
                             chunk-entry- channel-id timestamp size)
                            entry))
                      (add-to-queue channel-id
                                    (make-instance 'index-entry
                                                   :timestamp timestamp
                                                   :chunk-id  chunk-id
                                                   :offset    offset1))
                      (incf offset1 (+ 16 size)))))))
    (flush-indices)
    (values)))

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
