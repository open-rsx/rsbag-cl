;;;; repair.lisp --- Recover from damage in log files.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

(defun reconstruct-indices (stream chunks)
  "Reconstruct index information for CHUNKS based on the contents of
   STREAM. CHUNKS is a list of entries of the form

     (ID . OFFSET)

   . Return a list of corresponding `indx' instances."
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