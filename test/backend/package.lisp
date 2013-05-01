;;;; package.lisp --- Package definition for unit tests of the backend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsbag.backend.test
  (:use
   :cl
   :alexandria
   :let-plus
   :lift

   :rsbag.backend

   :rsbag.test)

  (:export
   :backend-root)

  (:documentation
   "This package contains unit tests for the backend module"))

(cl:in-package :rsbag.backend.test)

(deftestsuite backend-root (root)
  ()
  (:documentation
   "Root unit test suite for the backend module."))
