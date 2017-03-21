;;;; package.lisp --- Package definition for unit tests of the rsb module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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
   #:rsbag.rsb)

  (:export
   #:rsb-root)

  ;; Utilities
  (:export
   #:collecting-events)

  (:documentation
   "This package contains unit tests for the rsb module."))

(cl:in-package #:rsbag.rsb.test)

(defparameter *test-configuration*
  '(((:transport :inprocess :enabled) . t)
    ((:introspection :enabled)        . nil)))

(deftestsuite rsb-root (rsbag.test:root)
  ()
  (:dynamic-variables
   (rsb:*configuration* *test-configuration*))
  (:documentation
   "Root unit test suite for the rsb module."))

;;; Utilities

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
