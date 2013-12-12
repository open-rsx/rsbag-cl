;;;; fixed-rate.lisp --- Unit tests for the fixed-rate class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.test)

(deftestsuite fixed-rate-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `fixed-rate' replay strategy class."))

(define-replay-strategy-construction-test (fixed-rate)
  ;; Some invalid cases.
  '(()                                   missing-required-initarg)
  '((:delay 1 :rate 1)                   incompatible-initargs)
  '((:delay 0)                           type-error)
  '((:rate 0)                            type-error)

  ;; Some valid cases.
  '((:delay 1)                           t)
  `((:delay 1 :error-policy ,#'continue) t)
  '((:rate 1)                            t)
  `((:rate 1 :error-policy ,#'continue)  t))

(define-replay-strategy-smoke-test (fixed-rate
                                    :expected-var expected)
  ;; Some simple cases.
  ('(:rate  1000))
  (`(:rate  1000 :error-policy ,#'continue))
  ('(:delay 1/1000))
  (`(:delay 1/1000 :error-policy ,#'continue))

  ;; Without an error policy, the first failing event causes an error
  ;; to be signaled.
  ('(:rate 1000 :error-policy nil)
   :bag      (simple-bag :errors '(2))
   :expected 'entry-retrieval-error)
  ('(:rate 1000 :error-policy nil)
   :processing-errors '(2)
   :expected          'entry-processing-error)
  ;; The `continue' restart skips to the next entry. Therefore, the
  ;; observed output continues after the failing entry.
  (`(:rate 1000 :error-policy ,#'continue)
   :bag      (simple-bag :errors '(4))
   :expected (remove 4 expected))
  (`(:rate 1000 :error-policy ,#'continue)
   :processing-errors '(4)
   :expected          (remove 4 expected)))
