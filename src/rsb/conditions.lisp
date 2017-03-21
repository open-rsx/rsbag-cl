;;;; conditions.lisp --- Conditions used in the rsb module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

(define-condition connection-condition (rsbag-condition)
  ((connection :initarg  :connection
               :reader   connection-condition-connection
               :documentation
               "Stores the connection involved in the condition."))
  (:default-initargs
   :connection (missing-required-initarg 'connection-condition :connection))
  (:documentation
   "Subclasses of this condition are signaled when a bag connection is
    involved."))

(define-condition entry-condition (rsbag-condition)
  ((entry :initarg  :entry
          :reader   entry-condition-entry
          :documentation
          "Stores the entry involved in the condition."))
  (:default-initargs
   :entry (missing-required-initarg 'entry-condition :entry))
  (:documentation
   "Subclasses of this conditions are signaled when a bag entry is
    involved."))

;;; Recording conditions

(define-condition recording-error (connection-condition
                                   chainable-condition
                                   rsbag-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<A recording error related to ~A ~
                     occurred.~/more-conditions:maybe-print-cause/~@:>"
             (connection-condition-connection condition)
             condition)))
  (:documentation
   "Errors of this condition class and subclasses are signaled when
    errors occur during event recording into a bag."))

(define-condition entry-storage-error (entry-condition
                                       recording-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error storing entry ~A in ~
                     ~A.~/more-conditions:maybe-print-cause/~@:>"
             (entry-condition-entry           condition)
             (connection-condition-connection condition)
             condition)))
  (:documentation
   "This error is signaled when an entry cannot be stored during a
    recording process."))
