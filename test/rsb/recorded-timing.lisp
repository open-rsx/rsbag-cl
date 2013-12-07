;;;; recorded-timing.lisp --- Unit tests for the recorded-timing strategy class.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.test)

(deftestsuite recorded-timing-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `recorded-timing' replay strategy class."))

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
   (simple-bag :errors '(2))
   'event-retrieval-failed)
  ;; The `continue' restart skips to the next entry. Therefore, the
  ;; observed output continues after the failing entry.
  (`(:error-policy ,#'continue)
   (simple-bag :errors '(4))
   (remove 4 expected)))
