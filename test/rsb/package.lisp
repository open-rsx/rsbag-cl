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

(defmacro define-replay-strategy-construction-test
    ((class
      &key
      (suite-name (symbolicate class '#:-root)))
     &body
     cases)
  "Define a test case for class CLASS in test suite `SUITE-NAME' with
   CASES. Each element of CASES has to be of the form

     (INITARGS EXPECTED)

   where EXPECTED is either the symbol `error' or the symbol `t'."
  `(addtest (,suite-name
             :documentation
             ,(format nil "Test construction of `~(~A~)' instances." class))
     construction

     (ensure-cases (initargs expected)
         (list ,@cases)

       (let+ (((&flet do-it ()
                 (apply #'make-instance ',class initargs))))
         (case expected
           (missing-required-initarg
            (ensure-condition 'missing-required-initarg (do-it)))
           (incompatible-initargs
            (ensure-condition 'incompatible-initargs (do-it)))
           (type-error
            (ensure-condition 'type-error (do-it)))
           (error
            (ensure-condition 'error (do-it)))
           (t
            (do-it)))))))

(defmacro collecting-events ((name &key (errors ''())) &body body)
  "Execute BODY with a collector function named NAME in scope."
  (with-gensyms (collected)
    (once-only (errors)
      `(let+ ((,collected '())
              ((&flet ,name (&optional datum)
                 (cond
                   ((not datum)
                    (reverse ,collected))
                   ((not (member datum ,errors))
                    (push datum ,collected))
                   (t
                    (error "~@<Simulated processing error~@:>"))))))
         ,@body))))

(defun select-channel (name)
  (compose (curry #'string= name) #'channel-name))

(defun call-as-replay-strategy-test-case (bag processing-errors
                                          strategy-class strategy-initargs
                                          replay-function assessment-function)
  (let+ (((&flet do-it ()
            (collecting-events (collect :errors processing-errors)
              (with-open-connection
                  (connection (apply #'bag->events bag #'collect
                                     :replay-strategy strategy-class strategy-initargs))
                (funcall replay-function connection (connection-strategy connection)))
              (collect)))))
    (etypecase assessment-function
      ((eql error)
       (ensure-condition 'error (do-it)))
      ((eql replay-error)
       (ensure-condition 'replay-error (do-it)))
      ((eql entry-retrieval-error)
       (ensure-condition 'entry-retrieval-error (do-it)))
      ((eql entry-processing-error)
       (ensure-condition 'rsbag.rsb::entry-processing-error (do-it)))
      (function
       (funcall assessment-function (do-it))))))

(defmacro define-replay-strategy-smoke-test
    ((class
      &key
      (suite-name        (symbolicate class '#:-root))
      (bag-var           (gensym "BAG"))
      (initargs-var      (gensym "INITARGS"))
      (expected-var      (gensym "EXPECTED"))
      (required-initargs '()))
     &body
     cases)
  "Define a smoke test case for class CLASS in test suite `SUITE-NAME'
   with CASES. Each element of CASES has to be of the form

     (INITARGS &key BAG PROCESSING-ERRORS EXPECTED)

   where EXPECTED is the list of entries the strategy should produce
   when applied to BAG."
  `(addtest (,suite-name
             :documentation
             ,(format nil "Smoke test for the `~(~A~)' replay strategy class."
                      class))
     replay/smoke

     (let ((,bag-var (simple-bag)))
       (ensure-cases (initargs bag processing-errors expected)
           (list
            ,@(map-product
               (lambda+ ((initargs &key bag processing-errors expected)
                         (initargs1 expected1))
                 `(let ((,initargs-var (list ,@initargs1))
                        (,expected-var (list ,@expected1)))
                    (list (append ,initargs ,initargs-var)
                          ,@(if bag `(,bag) `(,bag-var))
                          ,processing-errors
                          ,@(if expected `(,expected) `(,expected-var)))))
               cases
               `((()                                     (1 2 3 4 5))
                 ((:end-index   5)                       (1 2 3 4 5))
                 ((:start-index 1)                       (2 3 4 5))
                 ((:end-index   3)                       (1 2 3))
                 ((:start-index -4)                      (2 3 4 5))
                 ((:end-index   -2)                      (1 2 3))
                 ((:start-index 1 :end-index 3)          (2 3))
                 ((:start-time  0.04)                    (2 3 4 5))
                 ((:start-time  -0.06)                   (2 3 4 5))
                 ((:end-time    0.09)                    (1 2 3 4))
                 ((:end-time    -0.02)                   (1 2 3 4))
                 ((:channels    (select-channel "/foo")) (1 2))))

            ;; Some invalid start and end indices.
            `((:start-index -10 ,,@required-initargs)
              ,,bag-var () replay-error)
            `((:start-index 10 ,,@required-initargs)
              ,,bag-var () replay-error)
            `((:start-index -10 :error-policy ,#'continue ,,@required-initargs)
              ,,bag-var () ())
            `((:start-index 10 :error-policy ,#'continue ,,@required-initargs)
              ,,bag-var () ())
            `((:end-index -10 ,,@required-initargs)
              ,,bag-var () replay-error)
            `((:end-index 10 ,,@required-initargs)
              ,,bag-var () replay-error)
            `((:end-index -10 :error-policy ,#'continue ,,@required-initargs)
              ,,bag-var () ())
            `((:end-index 10 :error-policy ,#'continue ,,@required-initargs)
              ,,bag-var () ()))

         (call-as-replay-strategy-test-case
          bag processing-errors ',class initargs
          (lambda (connection strategy) (replay connection strategy))
          (case expected
            ((error
              replay-error entry-retrieval-error entry-processing-error)
             expected)
            (t
             (lambda (events)
               (ensure-same events expected :test #'equal)))))))))
