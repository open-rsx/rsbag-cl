;;;; interactive.lisp --- Unit tests for the interactive strategy class.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.test)

(deftestsuite interactive-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `interactive' replay strategy class."))

;;; Utilities

(defun %make-two-way-string-stream (input-string)
 (let ((input  (make-string-input-stream input-string))
       (output (make-string-output-stream)))
   (values (make-two-way-stream input output) input output)))

(defun %make-emit-and-next-stream (num-events)
  (%make-two-way-string-stream
   (format nil "~{~*emitandnext~%~}emit~%"
           (make-list (1- num-events)))))

(define-replay-strategy-smoke-test (interactive
                                    :expected-var expected)
  (`(:stream       ,(%make-emit-and-next-stream (length expected))))
  (`(:stream       ,(%make-emit-and-next-stream (length expected))
     :error-policy nil))
  (`(:stream       ,(%make-emit-and-next-stream (length expected))
     :error-policy ,#'continue))

  ;; Without an error policy, the first failing event causes an error
  ;; to be signaled.
  (`(:stream       ,(%make-emit-and-next-stream (length expected))
     :error-policy nil)
   (simple-bag :errors '(2))
   'event-retrieval-failed)
  ;; We use "emitandnext" which does not advance in case of errors,
  ;; even if the `continue' restart is used. Therefore, the observed
  ;; output gets stuck at the failing event.
  (`(:stream       ,(%make-emit-and-next-stream (length expected))
     :error-policy ,#'continue)
   (simple-bag :errors '(4))
   (let ((end-index (position 4 expected)))
     (subseq expected 0 end-index))))
