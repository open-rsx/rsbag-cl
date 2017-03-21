;;;; conditions.lisp --- Conditions used in the rsb.replay module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

(define-condition replay-error (connection-condition
                                chainable-condition
                                rsbag-error)
  ((strategy :initarg  :strategy
             :reader   replay-error-strategy
             :documentation
             "Stores the replay strategy involved in the replay
              error."))
  (:default-initargs
   :strategy (missing-required-initarg 'replay-error :strategy))
  (:report
   (lambda (condition stream)
     (format stream "~@<A replay error related to connection ~A and ~
                     replay strategy ~A ~
                     occurred.~/more-conditions:maybe-print-cause/~@:>"
             (connection-condition-connection condition)
             (replay-error-strategy           condition)
             condition)))
  (:documentation
   "Errors of this condition class and subclasses are signaled when
    errors occur during replay of entries from a bag."))

(define-condition entry-retrieval-error (replay-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error retrieving next entry from ~A for ~
                     replay according to strategy ~
                     ~A.~/more-conditions:maybe-print-cause/~@:>"
             (connection-condition-connection condition)
             (replay-error-strategy           condition)
             condition)))
  (:documentation
   "This error is signaled when the retrieval of an entry from a bag
    for replay fails."))

(define-condition entry-processing-error (entry-condition
                                          replay-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error processing entry ~A from ~A during ~
                     replay according to strategy ~
                     ~A.~/more-conditions:maybe-print-cause/~@:>"
             (entry-condition-entry           condition)
             (connection-condition-connection condition)
             (replay-error-strategy           condition)
             condition)))
  (:documentation
   "This error is signaled when processing of an entry fails during
    replay."))
