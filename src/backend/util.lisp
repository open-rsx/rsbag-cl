;;;; util.lisp --- Utilities used in the backed module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend)

(defun print-offset (stream offset &optional colon? at?)
  "Write OFFSET onto STREAM in hexadecimal and decimal forms."
  (declare (ignore at? colon?))
  (format stream "0x~X (~:*~:D)" offset))

;;; Time-related utility functions

(declaim (inline uint64->timestamp timestamp->uint64))

(defun uint64->timestamp (value)
  (let+ (((&values secs nsecs) (truncate value 1000000000)))
    (local-time:unix-to-timestamp secs :nsec nsecs)))

(defun timestamp->uint64 (value)
  (let+ (((&accessors-r/o (secs  local-time:timestamp-to-unix)
                          (nsecs local-time:nsec-of))
          value))
    (declare (type non-negative-integer     secs)
             (type (integer 0 (1000000000)) nsecs))
    (+ (* (expt 10 9) secs) nsecs)))
