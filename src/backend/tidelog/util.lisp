;;;; util.lisp --- Utility functions for the TIDELog backend.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; IO-related utility functions

(defun file-position-error (stream)
  (error "~@<Failed to determine position in stream ~A.~@:>"
         stream))

(declaim (inline file-position-or-lose))
(defun file-position-or-lose (stream)
  (or (file-position stream)
      (file-position-error stream)))

(defun read-chunk-of-length (length stream
                             &optional
                             (buffer (nibbles:make-octet-vector length)))
  "Create a `simple-octet-vector' (unless BUFFER is supplied) of
   length LENGTH and read LENGTH from STREAM into it. Return the
   buffer."
  (let ((read (read-sequence buffer stream)))
    (unless (= read length)
      (cerror "Continue with incomplete block"
              "~@<Could only read ~:D byte~:P when trying to read a ~
               sequence of ~:D byte~:P at stream position ~
               ~/rsbag.backend:print-offset/.~@:>"
              read length (file-position stream))))
  buffer)

;;; Buffering

(defmacro make-or-reuse-instance (array class &rest initargs)
  (once-only (array)
    (with-unique-names (fill-pointer element)
      `(let* ((,fill-pointer (fill-pointer ,array))
              (,element      (when (> (array-total-size ,array) ,fill-pointer)
                               (aref ,array ,fill-pointer))))
         (if (typep ,element ',class)
             (progn
               (incf (fill-pointer ,array))
               (reinitialize-instance ,element ,@initargs))
             (let ((,element (make-instance ',class ,@initargs)))
               (vector-push-extend ,element ,array)
               ,element))))))
