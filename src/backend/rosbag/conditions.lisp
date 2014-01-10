;;;; conditions.lisp --- Conditions used in the rosbag backend.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag)

(define-condition rosbag-condition (condition)
  ()
  (:documentation
   "This condition class serves as a superclass for Rosbag-related
    condition classes."))

(define-condition rosbag-file-error (log-file-error
                                     rosbag-condition)
  ()
  (:documentation
   "Errors of this class and subclasses are signaled when operations
    involving Rosbag log files fail."))

(define-condition invalid-rosbag-structure (invalid-file-structure
                                            rosbag-condition)
  ()
  (:documentation
   "This error is signaled if an invalid file structure is encountered
    while processing a Rosbag log file."))

(define-condition no-such-record-class-error (rosbag-condition
                                              error)
  ((opcode :initarg :opcode
           :type    octet
           :reader  no-such-record-class-error-opcode))
  (:default-initargs
   :opcode (missing-required-initarg 'no-such-record-class-error :opcode))
  (:report
   (lambda (condition stream)
     (format stream "~@<Unknown record opcode ~D.~@[ Known opcodes are: ~
                     ~{~{~D -> ~A~}~^, ~:_~}~]~@:>"
             (no-such-record-class-error-opcode condition)
             (mapcar (lambda+ ((opcode . class))
                       (list opcode (class-name class)))
                     (hash-table-alist *record-classes*)))))
  (:documentation
   "This error is signaled when an attempt is made to map an opcode
    does not correspond to a record type to a record class."))
