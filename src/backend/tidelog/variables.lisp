;;;; variables.lisp --- Variables used in the TIDE log file format backend.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; Version variables

(defconstant +format-version-major+ 0
  "Major version of the TIDE log file format supported by this
   backend.")

(defconstant +format-version-minor+ 0
  "Minor version of the TIDE log file format supported by this
   backend.")

;;; Known block classes

(declaim (type hash-table *byte-pattern->block-class*))

(defvar *byte-pattern->block-class*
  (make-hash-table :test #'equalp)
  "Stores a mapping from block tags as `octet-vecor' patterns to block
   classes.")

(declaim (ftype (function (nibbles:simple-octet-vector &key (:if-does-not-exist t))
                          (values class &optional))
                byte-pattern->block-class))

(defun byte-pattern->block-class (pattern &key (if-does-not-exist #'error))
  "Return the block class corresponding to PATTERN.

   Act according to IF-DOES-NOT-EXIST, if there is no such class."
  (or (gethash pattern *byte-pattern->block-class*)
      (error-behavior-restart-case
          (if-does-not-exist (no-such-block-class-error :tag pattern)))))
