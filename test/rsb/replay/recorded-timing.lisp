;;;; recorded-timing.lisp --- Unit tests for the recorded-timing strategy class.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay.test)

(deftestsuite recorded-timing-root (rsb-replay-root)
  ()
  (:documentation
   "Test suite for the `recorded-timing' replay strategy class."))

(define-replay-strategy-construction-test (recorded-timing)
  ;; Some invalid cases.
  '((:speed 0)                               type-error)
  '((:speed -1)                              type-error)
  '((:max-delay -1)                          type-error)

  ;; Some valid cases.
  '(()                                       t)
  '((:speed 1)                               t)
  `((:speed 0.5 :error-policy ,#'continue)   t)
  `((:speed 3/2 :error-policy nil)           t)
  '((:max-delay 0)                           t)
  `((:max-delay 1 :error-policy ,#'continue) t)
  `((:max-delay 1/2 :error-policy nil)       t))

(define-replay-strategy-smoke-test (recorded-timing
                                    :expected-var expected)
  ;; Some simple cases.
  ('())
  (`(:error-policy ,#'continue))
  ('(:speed 2.5))
  (`(:speed 1/2 :error-policy ,#'continue))
  ('(:max-delay 1))
  (`(:max-delay 1d-9 :error-policy ,#'continue))

  ;; Without an error policy, the first failing event causes an error
  ;; to be signaled.
  ('(:error-policy nil)
   :bag      (rsbag.test:simple-bag :errors '(2))
   :expected 'entry-retrieval-error)
  ('(:error-policy nil)
   :processing-errors '(2)
   :expected          'entry-processing-error)

  ;; The `continue' restart skips to the next entry. Therefore, the
  ;; observed output continues after the failing entry.
  (`(:error-policy ,#'continue)
   :bag      (rsbag.test:simple-bag :errors '(4))
   :expected (remove 4 expected))
  (`(:error-policy ,#'continue)
   :processing-errors '(4)
   :expected          (remove 4 expected)))
