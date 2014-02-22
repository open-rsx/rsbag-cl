;;;; conditions.lisp --- Conditions used in the TIDE log backend of cl-rsbag.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

(define-condition tidelog-condition (rsbag-condition)
  ()
  (:documentation
   "This condition class serves as a superclass for TIDELOG-related
    condition classes."))

(define-condition tidelog-file-error (log-file-error
                                      tidelog-condition)
  ()
  (:documentation
   "Errors of this class and subclasses are signaled when operations
    involving TIDE log files fail."))

(define-condition invalid-tidelog-structure (invalid-file-structure
                                             tidelog-condition)
  ()
  (:documentation
   "This error is signaled if an invalid file structure is encountered
    while processing a TIDE log file."))

(define-condition no-such-block-class-error (tidelog-condition
                                             rsbag-error)
  ((tag :initarg :tag
        :type    nibbles:octet-vector
        :reader  no-such-block-class-error-tag
        :documentation
        "Stores the unknown block type tag as an `octet-vector'."))
  (:default-initargs
   :tag (missing-required-initarg 'no-such-block-class-error :tag))
  (:report
   (lambda (condition stream)
     (format stream "~@<Unknown block tag~:@_~
                     ~<| ~@;~17/rsbag:print-hexdump/~:>~@:_~
                     . Known block tags are: ~{~A~^, ~:_~}.~:>"
             (list (no-such-block-class-error-tag condition))
             (mapcar #'class-name (hash-table-values
                                   *byte-pattern->block-class*)))))
  (:documentation
   "This error is signaled when an attempt is made to map a byte
    pattern which does not correspond to a block type to a block
    class."))
