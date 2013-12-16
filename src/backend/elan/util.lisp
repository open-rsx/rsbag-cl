;;;; util.lisp --- Utilities used by the Elan backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.elan)

(defun parse/keep-open (stream builder)
  "Similar to `cxml:parse' but does not close STREAM."
  (cxml:parse (make-two-way-stream stream (make-broadcast-stream))
              builder))

(defun serialize/keep-open (document stream)
  "Similar to `stp:serialize' but does not close STREAM."
  (stp:serialize document (cxml:make-octet-stream-sink
                           (make-broadcast-stream stream))))
