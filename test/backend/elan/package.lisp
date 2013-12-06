;;;; package.lisp --- Package definition for unit tests of the ELAN backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.elan.test
  (:use
   #:cl
   #:lift

   #:rsbag.backend.elan

   #:rsbag.backend.test)

  (:export
   #:backend-elan-root)

  (:documentation
   "This package contains unit tests for the ELAN backend module."))

(cl:in-package #:rsbag.backend.elan.test)

(deftestsuite backend-elan-root (backend-root)
  ()
  (:documentation
   "Root unit test suite for the ELAN backend."))
