;;;; package.lisp --- Package definition for unit tests of the view module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.view.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:lift

   #:rsbag
   #:rsbag.view

   #:rsbag.test)

  (:export
   #:view-root)

  (:documentation
   "This package contains unit tests for the view module."))

(cl:in-package #:rsbag.view.test)

(deftestsuite view-root (root)
  ()
  (:documentation
   "Root unit test suite for the view module."))
