;;;; package.lisp --- Package definition for unit tests of the backend.rosbag module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.rosbag.test
  (:use
   #:cl
   #:alexandria
   #:lift

   #:nibbles
   #:flexi-streams)

  (:shadowing-import-from #:nibbles
   #:octet)

  (:import-from
   #:rsbag.backend.rosbag
   #:size
   #:scan
   #:unpack)

  (:export
   #:rsbag.backend.rosbag.root)

  (:documentation
   "This package contains unit tests for the backend.rosbag module"))

(cl:in-package #:rsbag.backend.rosbag.test)

(deftestsuite rsbag.backend.rosbag.root (#+later backend-root #+later rsbag.backend.root)
  ()
  (:documentation
   "Root unit test suite for the backend.rosbag module."))
