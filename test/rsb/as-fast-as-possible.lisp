;;;; as-fast-as-possible.lisp --- Unit tests for the as-fast-as-possible strategy class.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.test)

(deftestsuite as-fast-as-possible-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `as-fast-as-possible' replay strategy class."))

(define-replay-strategy-construction-test (as-fast-as-possible)
   ;; There are few interesting cases here.
  '(()                          t)
  `((:error-policy ,#'continue) t)
  '((:error-policy nil)         t))

(define-replay-strategy-smoke-test (as-fast-as-possible
                                    :expected-var expected)
  ;; Some simple cases.
  ('())
  (`(:error-policy ,#'continue))

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
