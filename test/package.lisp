;;;; package.lisp --- Package definition for unit tests of the cl-rsbag system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:lift

   #:rsbag)

  (:export
   #:root)

  ;; Test utilities
  (:export
   #:mock-backend

   #:with-mock-backend
   #:with-mock-bag

   #:simple-bag)

  (:documentation
   "This package contains unit tests for the cl-rsbag system"))

(cl:in-package #:rsbag.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the cl-rsbag system."))
