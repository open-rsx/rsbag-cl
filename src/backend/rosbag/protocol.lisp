;;;; protocol.lisp --- Protocol functions provided by the backend.rosbag module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag)

;;; Block IO protocol

(defgeneric size (object)
  (:documentation
   "TODO"))

(defgeneric scan (source object &optional start)
  (:documentation
   "TODO"))

(defgeneric unpack (source object &optional start)
  (:documentation
   "TODO"))

(defgeneric pack (object destination &optional start)
  (:documentation
   "TODO"))
