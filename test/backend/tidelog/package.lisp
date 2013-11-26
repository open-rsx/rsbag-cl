;;;; package.lisp --- Package definition for unit tests of the tidelog backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.tidelog.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:lift

   #:rsbag.backend.tidelog

   #:rsbag.backend.test)

  (:import-from #:rsbag.backend.tidelog
   #:byte-pattern->block-class

   #:find-next-block

   #:+format-version-major+
   #:+format-version-minor+

   #:scan ; TODO remove when exported
   )

  (:export
   #:backend-tidelog-root)

  (:documentation
   "This package contains unit tests for the backend module"))

(cl:in-package #:rsbag.backend.tidelog.test)

(deftestsuite backend-tidelog-root (backend-root)
  ()
  (:documentation
   "Root unit test suite for the tidelog backend."))

;;; Utilities

(defun tide-block (&key (version-major +format-version-major+)
                        (version-minor +format-version-minor+))
  `(:tide (:ub64le 0) ,version-major ,version-minor (:ub32le 0) (:ub32le 0)))

(defun valid-chnk-block (&key (id 0) (count 0) (content '()))
  (let ((content-size (length (apply #'octetify content))))
    `(:chnk (:ub64le ,(+ 25 content-size)) (:ub32le ,id) (:ub32le ,count)
      (:ub64le 0) (:ub64le 0) 0
      ,@content)))

(defparameter +invalid-chnk-block+
  `(:chnk (:ub64le ,(ash 1 63)) (:ub32le 0)))
