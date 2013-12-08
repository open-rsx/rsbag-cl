;;;; package.lisp --- Package definition for unit tests of the backend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:rsbag.backend

   #:rsbag.test)

  ;; Root test suite
  (:export
   #:backend-root)

  ;; Test utilities
  (:export
   #:octetify
   #:octet-streamify)

  (:documentation
   "This package contains unit tests for the backend module"))

(cl:in-package #:rsbag.backend.test)

(deftestsuite backend-root (root)
  ()
  (:documentation
   "Root unit test suite for the backend module."))

;;; Test utilities

(defun octetify (&rest things)
  (labels ((one (thing)
             (etypecase thing
               (keyword       (one (string thing)))
               (string        (map 'nibbles:octet-vector #'char-code thing))
               (sequence      (coerce thing 'nibbles:octet-vector))
               (nibbles:octet (nibbles:octet-vector thing)))))
    (apply #'concatenate 'nibbles:octet-vector (mapcar #'one things))))

(defun octet-streamify (&rest things)
  (flexi-streams:make-in-memory-input-stream
   (apply #'octetify things)))
