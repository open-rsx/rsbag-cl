;;;; package.lisp --- Package definition for unit tests of the tidelog backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.tidelog.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:lift

   #:rsbag.backend.tidelog

   #:rsbag.backend.test)

  (:import-from #:rsbag.backend.tidelog
   #:byte-pattern->block-class

   #:find-next-block)

  (:export
   #:backend-tidelog-root)

  (:documentation
   "This package contains unit tests for the backend module"))

(cl:in-package #:rsbag.backend.tidelog.test)

(deftestsuite backend-tidelog-root (backend-root)
  ()
  (:documentation
   "Root unit test suite for the tidelog backend."))
