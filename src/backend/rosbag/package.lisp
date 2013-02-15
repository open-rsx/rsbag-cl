;;;; package.lisp --- Package definition for the backend.rosbag module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.rosbag
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:nibbles

   #:rsbag.backend)

  ;; Conditions
  (:export
   #:rosbag-condition
   #:rosbag-file-error
   #:invalid-rosbag-structure)

  (:documentation
   "This package contains a backend for Version 2.0 of the Rosbag file
    format as specified at http://www.ros.org/wiki/Bags/Format/2.0.

    TODO overview"))
