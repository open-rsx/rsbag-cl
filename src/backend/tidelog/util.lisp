;;;; util.lisp --- Utility functions for the TIDELog backend.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; IO-related utility functions

(defun read-chunk-of-length (length stream
                             &optional
                             (buffer (nibbles:make-octet-vector length)))
  "Create a `simple-octet-vector' (unless BUFFER is supplied) of
length LENGTH and read LENGTH from STREAM into it. Return the buffer."
  (let ((read (read-sequence buffer stream)))
    (unless (= read length)
      (cerror "Continue with incomplete block"
              "~@<Could only read ~:D byte~:P when trying to read a ~
               sequence of ~:D byte~:P at stream position ~:D.~@:>"
              read length (file-position stream))))
  buffer)

;;; Time-related utility functions

(defun uint64->timestamp (value)
  (let+ (((&values secs nsecs) (truncate value 1000000000)))
    (local-time:unix-to-timestamp secs :nsec nsecs)))

(defun timestamp->uint64 (value)
  (let+ (((&accessors-r/o (secs  local-time:timestamp-to-unix)
                          (nsecs local-time:nsec-of)) value))
    (declare (type non-negative-integer     secs)
             (type (integer 0 (1000000000)) nsecs))
    (+ (* (expt 10 9) secs) nsecs)))
