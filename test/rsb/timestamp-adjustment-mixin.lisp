;;;; timestamp-adjustment-mixin.lisp --- Unit tests for the timestamp-adjustment-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.test)

(deftestsuite timestamp-adjustment-mixin-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `timestamp-adjustment-mixin' replay strategy
class."))

(addtest (timestamp-adjustment-mixin-root
          :documentation
          "Test construction of `timestamp-adjustment-mixin' instances.")
  construction

  (ensure-cases (args expected)
      `(;; These are OK
        (()                                                        t)
        ((:adjustments ())                                         t)
        ((:adjustments ((:create :now)))                           t)
        ((:adjustments ((:create ,(local-time:now))))              t)
        ((:adjustments ((:create ,(local-time:now)) (:send :now))) t)
        ((:adjustments ((:send   (:copy :create))))                t)

        ;; invalid syntax
        ((:adjustments :foo)                                       error)
        ((:adjustments (:foo))                                     error)
        ((:adjustments ((:foo)))                                   error)
        ((:adjustments ((:create (:copy))))                        error))

  (case expected
    (error
     (ensure-condition 'error
       (apply #'make-instance 'timestamp-adjustment-mixin args)))
    (t
     (apply #'make-instance 'timestamp-adjustment-mixin args)))))
