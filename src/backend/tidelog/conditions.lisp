;;;; conditions.lisp --- Conditions used in the TIDE log backend of cl-rsbag.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.backend.tidelog)

(define-condition tidelog-condition (condition)
  ()
  (:documentation
   "This condition class serves as a superclass for TIDELOG-related
condition classes."))

(define-condition tidelog-file-error (log-file-error
				      tidelog-condition)
  ()
  (:documentation
   "Errors of this class and subclasses are signaled when operations
involving TIDE log files fail."))

(define-condition invalid-tidelog-structure (invalid-file-structure
					     tidelog-condition)
  ()
  (:documentation
   "This error is signaled if an invalid file structure is encountered
while processing a TIDE log file."))
