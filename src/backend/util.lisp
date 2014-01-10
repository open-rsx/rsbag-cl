;;;; util.lisp --- Utilities used in the backed module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend)

(defun print-offset (stream offset &optional colon? at?)
  "Write OFFSET onto STREAM in hexadecimal and decimal forms."
  (declare (ignore at? colon?))
  (format stream "0x~X (~:*~:D)" offset))
