;;;; conditions.lisp --- Conditions used in the rosbag backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag)

(define-condition rosbag-condition (condition)
  ()
  (:documentation
   "This condition class serves as a superclass for Rosbag-related
    condition classes."))

(define-condition rosbag-file-error (log-file-error
                                     rosbag-condition)
  ()
  (:documentation
   "Errors of this class and subclasses are signaled when operations
    involving Rosbag log files fail."))

(define-condition invalid-rosbag-structure (invalid-file-structure
                                            rosbag-condition)
  ()
  (:documentation
   "This error is signaled if an invalid file structure is encountered
    while processing a Rosbag log file."))
