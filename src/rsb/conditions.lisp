;;;; conditions.lisp --- Conditions used in the rsb module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb)

(define-condition connection-error (error)
  ((connection :initarg  :connection
               :reader   connection-error-connection
               :documentation
               "Stores the connection involved in the error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<An error related to connection ~A ~
occurred.~@:>"
             (connection-error-connection condition))))
  (:documentation
   "Errors of this condition class and subclasses are signaled when
errors involving bag-connections occur."))

(define-condition recording-error (connection-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<A recording error related to ~A occurred.~@:>"
             (connection-error-connection condition))))
  (:documentation
   "Errors of this condition class and subclasses are signaled when
errors occur during event recording into a bag."))

(define-condition event-storage-failed (recording-error
                                        chainable-condition)
  ((event :initarg  :event
          :reader   connection-error-event
          :documentation
          "Stores the event that could not be stored."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Event ~A could not be stored in ~
~A.~/more-conditions::maybe-print-cause/~@:>"
             (connection-error-event      condition)
             (connection-error-connection condition)
             condition)))
  (:documentation
   "This error is signaled when an event cannot be stored during a
recording process."))

(define-condition replay-error (connection-error)
  ((strategy :initarg  :strategy
             :reader   connection-error-strategy
             :documentation
             "Stores the replay strategy involved in the replay
error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<A replay error related to source ~A and replay ~
strategy ~A occurred.~@:>"
             (connection-error-connection condition)
             (connection-error-strategy   condition))))
  (:documentation
   "Errors of this condition class and subclasses are signaled when
errors occur during replay of events from a bag."))

(define-condition event-retrieval-failed (replay-error
                                          chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to retrieve next event from ~A for ~
replay according to strategy ~A~/more-conditions::maybe-print-cause/~@:>"
             (connection-error-connection condition)
             (connection-error-strategy   condition)
             condition)))
  (:documentation
   "This error is signaled when the retrieval of an event from a bag
for replay fails."))
