;;;; variables.lisp --- Variables in the backend.rosbag module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
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
  "Maps record opcodes to record class names.")

(defun find-record-class (opcode)
  (or (gethash opcode *record-classes*)
      (error "~@<No record class for opcode ~D.~@:>" opcode)))

(defun (setf find-record-class) (new-value opcode)
  (setf (gethash opcode *record-classes*) new-value))
