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

(addtest (fixed-rate-root
          :documentation
          "Test construction of `fixed-rate' instances.")
  construction

  (ensure-cases (args)
      '(()
        (:delay 1 :rate 1)
        (:delay 0)
        (:rate 0))

    (ensure-condition 'error
      (apply #'make-instance 'fixed-rate args))))

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
   (simple-bag :errors '(2))
   'event-retrieval-failed)
  ;; The `continue' restart skips to the next entry. Therefore, the
  ;; observed output continues after the failing entry.
  (`(:rate 1000 :error-policy ,#'continue)
   (simple-bag :errors '(4))
   (remove 4 expected)))
