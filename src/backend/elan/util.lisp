;;;; util.lisp --- Utilities used by the Elan backend.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.elan)

;;; Utilities for cxml

(defun parse/keep-open (stream builder)
  "Similar to `cxml:parse' but does not close STREAM."
  (cxml:parse (make-two-way-stream stream (make-broadcast-stream))
              builder))

(defun serialize/keep-open (document stream)
  "Similar to `stp:serialize' but does not close STREAM."
  (stp:serialize document (cxml:make-octet-stream-sink
                           (make-broadcast-stream stream))))

;;; Timestamp utilities

(declaim (inline milliseconds->nanoseconds nanoseconds->milliseconds))

(defun milliseconds->nanoseconds (value)
  (* value 1000000))

(defun nanoseconds->milliseconds (value)
  (floor value 1000000))

(defun nanoseconds->timestamp (value)
  (let+ (((&values secs nsecs) (truncate value 1000000000)))
    (local-time:unix-to-timestamp secs :nsec nsecs)))

(defun timestamp->nanoseconds (value)
  (let+ (((&accessors-r/o (secs  local-time:timestamp-to-unix)
                          (nsecs local-time:nsec-of))
          value))
    (+ (* 1000000000 secs) nsecs)))
