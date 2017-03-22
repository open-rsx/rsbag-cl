;;;; conditions.lisp --- Conditions used in the rsb module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

(define-condition connection-condition (rsbag-condition)
  ((connection :initarg  :connection
               :reader   connection-condition-connection
               :documentation
               "Stores the connection involved in the condition."))
  (:default-initargs
   :connection (missing-required-initarg 'connection-condition :connection))
  (:documentation
   "Subclasses of this condition are signaled when a bag connection is
    involved."))

(define-condition entry-condition (rsbag-condition)
  ((entry :initarg  :entry
          :reader   entry-condition-entry
          :documentation
          "Stores the entry involved in the condition."))
  (:default-initargs
   :entry (missing-required-initarg 'entry-condition :entry))
  (:documentation
   "Subclasses of this conditions are signaled when a bag entry is
    involved."))
