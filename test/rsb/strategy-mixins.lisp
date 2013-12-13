;;;; strategy-mixins.lisp --- Unit tests for replay strategy mixin classes.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.test)

;;; `bounds-mixin'

(deftestsuite bounds-mixin-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `bounds-mixin' replay strategy mixin class."))

(define-replay-strategy-construction-test (bounds-mixin)
  ;; Some invalid cases.
  '((:start-index 1.5)              type-error)
  '((:end-index 1/2)                type-error)
  '((:start-index 2 :end-index 1)   incompatible-initargs)
  '((:start-index -1 :end-index -2) incompatible-initargs)

  ;; These are OK.
  '((:start-index 0)                t)
  '((:start-index -10)              t)
  '((:end-index 1)                  t)
  '((:end-index -1)                 t)
  '((:start-index 1 :end-index 2)   t))

;;; `time-bounds-mixin'

(deftestsuite time-bounds-mixin-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `time-bounds-mixin' replay strategy mixin
    class."))

(define-replay-strategy-construction-test (time-bounds-mixin)
  ;; Some invalid cases.
  ;; TODO check types?
  '((:start-time 1.0 :start-index 2)                            incompatible-initargs)
  '((:end-time -1.0 :end-index 2)                               incompatible-initargs)

  ;; These are OK.
  '((:start-time 1.5)                                           t)
  '((:end-time -1)                                              t)
  '((:start-time 1/2 :end-time 1)                               t)
  `((:start-time ,(local-time:now))                             t)
  `((:end-time ,(local-time:now))                               t)
  (let* ((now   (local-time:now))
         (later (local-time:adjust-timestamp now (:offset :sec 1))))
    `((:start-time ,now :end-time ,later) t))
  '((:start-time 1 :end-index 5)                                t)
  `((:end-time ,(local-time:now) :start-index 5)                t))

;;; `timestamp-adjustment-mixin'

(deftestsuite timestamp-adjustment-mixin-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `timestamp-adjustment-mixin' replay strategy
    mixin class."))

(define-replay-strategy-construction-test (timestamp-adjustment-mixin)
  ;; Some invalid cases.
  '((:adjustments :foo)                                       type-error)
  '((:adjustments (:foo))                                     type-error)
  '((:adjustments ((:foo)))                                   type-error)
  '((:adjustments ((:create (:copy))))                        type-error)

  ;; These are OK.
  '(()                                                        t)
  '((:adjustments ())                                         t)
  '((:adjustments ((:create :now)))                           t)
  `((:adjustments ((:create ,(local-time:now))))              t)
  `((:adjustments ((:create ,(local-time:now)) (:send :now))) t)
  '((:adjustments ((:send   (:copy :create))))                t))
