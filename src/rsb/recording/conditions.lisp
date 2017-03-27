;;;; conditions.lisp --- Conditions used in the rsb.recording module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.recording)

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
