;;;; package.lisp --- Package definition for unit tests of the rsb module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.rsb.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions
   #:lift

   #:nibbles

   #:rsbag
   #:rsbag.rsb
   #:rsbag.rsb.replay

   #:rsbag.test)

  (:documentation
   "This package contains unit tests for the rsb module."))

(cl:in-package #:rsbag.rsb.test)

(deftestsuite rsb-root (root)
  ()
  (:documentation
   "Root unit test suite for the rsb module."))

;;; Utilities

(defmacro collecting-events ((name) &body body)
  "Execute BODY with a collector function named NAME in scope."
  (with-gensyms (collected)
    `(let+ ((,collected '())
            ((&flet ,name (&optional datum)
               (if datum
                   (push datum ,collected)
                   (reverse ,collected)))))
       ,@body)))

(defun select-channel (name)
  (compose (curry #'string= name) #'channel-name))

(defun call-as-replay-strategy-test-case (bag strategy-class strategy-initargs
                                          replay-function assessment-function)
  (let+ (((&flet do-it ()
            (collecting-events (collect)
              (with-open-connection
                  (connection (apply #'bag->events bag #'collect
                                     :replay-strategy strategy-class strategy-initargs))
                (funcall replay-function connection (connection-strategy connection)))
              (collect)))))
    (etypecase assessment-function
      ((eql error)
       (ensure-condition 'error (do-it)))
      ((eql event-retrieval-failed)
       (ensure-condition 'event-retrieval-failed (do-it)))
      (function
       (funcall assessment-function (do-it))))))

(defmacro define-replay-strategy-smoke-test
    ((class
      &key
      (suite-name   (symbolicate class '#:-root))
      (bag-var      (gensym "BAG"))
      (initargs-var (gensym "INITARGS"))
      (expected-var (gensym "EXPECTED")))
     &body
     cases)
  "Define a smoke test case for class CLASS in test suite `SUITE-NAME'
   with CASES. Each element of CASES has to be of the form

     (INITARGS &optional BAG EXPECTED)

   where EXPECTED is the list of entries the strategy should produce
   when applied to BAG."
  `(addtest (,suite-name
             :documentation
             ,(format nil "Smoke test for the `~(~A~)' replay strategy class."
                      class))
     replay/smoke

     (let ((,bag-var (simple-bag)))
       (ensure-cases (initargs bag expected)
           (list
            ,@(map-product
               (lambda+ ((initargs &optional bag expected) (initargs1 expected1))
                 `(let ((,initargs-var (list ,@initargs1))
                        (,expected-var (list ,@expected1)))
                    (list (append ,initargs ,initargs-var)
                          ,@(if bag `(,bag) `(,bag-var))
                          ,@(if expected `(,expected) `(,expected-var)))))
               cases
               `((()                                     (1 2 3 4 5))
                 ((:end-index   5)                       (1 2 3 4 5))
                 ((:start-index 1)                       (2 3 4 5))
                 ((:end-index   3)                       (1 2 3))
                 ((:channels    (select-channel "/foo")) (1 2)))))

         (call-as-replay-strategy-test-case
          bag ',class initargs
          (lambda (connection strategy) (replay connection strategy))
          (case expected
            ((error event-retrieval-failed)
             expected)
            (t
             (lambda (events)
               (ensure-same events expected :test #'equal)))))))))
