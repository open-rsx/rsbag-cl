;;;; package.lisp --- Package definition for unit tests of the rsbag system.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
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
   "This package contains unit tests for the rsbag system"))

(cl:in-package #:rsbag.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the rsbag system."))
