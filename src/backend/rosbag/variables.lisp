;;;; variables.lisp --- Variables in the backend.rosbag module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag)

;;;

(defvar +header/2.0+ (concatenate
                      'simple-octet-vector
                      (map 'list #'char-code "#ROSBAG V2.0") '(10))
  "TODO(jmoringe): document")

;;; Record class registry

(defvar *record-classes* (make-hash-table)
  "Maps record opcodes to record classes.")

(defun find-record-class (opcode &key (if-does-not-exist #'error))
  (or (gethash opcode *record-classes*)
      (error-behavior-restart-case
          (if-does-not-exist (no-such-record-class-error :opcode opcode)))))

(defun (setf find-record-class) (new-value opcode)
  (setf (gethash opcode *record-classes*) new-value))
