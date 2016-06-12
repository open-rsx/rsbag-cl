;;;; package.lisp --- Package definition for unit tests of the tidelog backend.
;;;;
;;;; Copyright (C) 2013, 2015, 2016 Jan Moringen
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

   #:reconstruct-indices

   #:find-next-block

   #:+format-version-major+
   #:+format-version-minor+

   #:indx-channel-id
   #:indx-count

   #:index-add-entries)

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

(defun chunk-entry ()
  `((:ub32le 0) (:ub64le 0) (:ub32le 1) 0))

(defparameter +invalid-chnk-block+
  `(:chnk (:ub64le ,(ash 1 63)) (:ub32le 0)))

(defun call-with-writable-log (function &key (close t))
  (let* ((stream  (flexi-streams:make-in-memory-output-stream))
         (backend (rsbag.backend:make-backend :tide
                                              :stream    stream
                                              :direction :output)))
    (multiple-value-call #'values
      (funcall function backend)
      (progn
        (when close (close backend))
        (flexi-streams:get-output-stream-sequence stream))
      backend)))

(defmacro with-writable-log ((backend-var &key (close nil close-supplied?))
                             &body body)
  `(call-with-writable-log (lambda (,backend-var) ,@body)
                           ,@(when close-supplied? `(:close ,close))))

(defun call-with-readable-log (function data)
  (let* ((stream  (flexi-streams:make-in-memory-input-stream data))
         (backend (rsbag.backend:make-backend :tide
                                              :stream    stream
                                              :direction :input)))
    (multiple-value-call #'values (funcall function backend) backend)))

(defmacro with-readable-log ((backend-var data) &body body)
  `(call-with-readable-log (lambda (,backend-var) ,@body) ,data))
