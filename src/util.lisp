;;;; util.lisp --- Utilities used in the rsbag system.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defmacro define-plist-data-mixin (name
                                   &key
                                   (slot-name name))
  "Define a class `plist-NAME-mixin' which manages a plist in a
   slot. Define the following accessors along with the class:
   + `NAME-count' :: Return number of items.
   + `NAME-keys' :: Return item keys.
   + `NAME-values' :: Return item values.
   + `NAME-plist' :: Return items as plist.
   + `NAME-alist' :: Return items as alist."
  (let+ ((class-name (symbolicate "PLIST-" name "-MIXIN"))
         (initarg    (make-keyword slot-name))
         ((count-name keys-name values-name plist-name alist-name)
          (map 'list (curry #'symbolicate name)
               '("-COUNT" "-KEYS" "-VALUES" "-PLIST" "-ALIST"))))
    `(progn
       (defclass ,class-name ()
         ((,slot-name :initarg  ,initarg
                      :type     list
                      :initform nil
                      :documentation

                      ,(format nil "Stores the ~(~A~) items associated ~
                                    to the instance."
                               name)))
         (:documentation
          "This mixin adds storage for a plist of items and associated
           accessors. See `define-plist-data-mixin' for a
           description."))

       (defgeneric ,count-name (object)
         (:method ((object ,class-name))
           (ash (length (slot-value object ',slot-name)) -1))
         (:documentation
          ,(format nil "Return the number of ~(~A~) items stored in OBJECT."
                   name)))

       (defgeneric ,keys-name (object)
         (:method ((object ,class-name))
           (iter (for (key) on (slot-value object ',slot-name)
                      :by #'cddr)
                 (collect key)))
         (:documentation

          ,(format nil "Return a list of the keys of ~(~A~) items ~
                        stored in OBJECT."
                   name)))

       (defgeneric ,values-name (object)
         (:method ((object ,class-name))
           (iter (for (key value) on (slot-value object ',slot-name)
                      :by #'cddr)
                 (collect value)))
         (:documentation
          ,(format nil "Return a list of the values of ~(~A~) items ~
                        stored in OBJECT."
                   name)))

       (defgeneric ,plist-name (object)
         (:method ((object ,class-name))
           (slot-value object ',slot-name))
         (:documentation
          ,(format nil "Return a plist of the ~(~A~) items stored in ~
                        OBJECT."
                   name)))

       (defgeneric ,alist-name (object)
         (:method ((object ,class-name))
           (plist-alist (slot-value object ',slot-name)))
         (:documentation
          ,(format nil "Return an alist of the ~(~A~) items stored ~
                        in OBJECT."
                   name)))

       (defgeneric ,name (object key)
         (:method ((object ,class-name) (key t))
           (getf (slot-value object ',slot-name) key))
         (:documentation
          ,(format nil "Return the ~(~A~) item of OBJECT identified ~
                        by KEY."
                   name)))

       (defgeneric (setf ,name) (new-value object key)
         (:method ((new-value t) (object ,class-name) (key t))
           (setf (getf (slot-value object ',slot-name) key) new-value))
         (:documentation

          ,(format nil "Associate NEW-VALUE to OBJECT as the ~(~A~)
                        item identified by KEY."
                   name))))))

(define-plist-data-mixin meta-data)

;;; Error handling utilities

(defmacro function-calling-restart-bind (clauses &body body)
  "Execute BODY with established restarts according to CLAUSES.

   Each element of clauses is of the form

     ((NAME LAMBDA-LIST &rest VARIABLES) &key REPORT)

   where

     NAME is the name of the restart that should be established

     VARIABLES is a list of variables names which are initially bound
     to nil but can be set to functions of one parameter - a condition
     instance - in BODY which implement the behavior of the restart.

     The keyword parameter REPORT works like :report-function in
     `cl:restart-case'.

   Restarts are only active if one of their VARIABLES in non-nil. When
   such a restart is invoked, the first non-nil variable among its
   VARIABLES is called as function with the condition as its sole
   argument to implement the behavior of the restart.

   Example:

     (control-transferring-restart-bind
       (((continue skip)
         :report (lambda (stream) (format stream \"Skip the element\"))))
       (iter (when (first-iteration-p)
               (setf skip (lambda () (next-iteration))))
             DO-SOMETHING)) "
  (let+ ((all-variables '())
         ((&flet+ process-clause (((name &ign &rest variables)
                                   &key report))
            (mapc (lambda (var) (pushnew var all-variables)) variables)
            `(,name (lambda (&rest args)
                      (apply (or ,@variables) args))
                    :test-function (lambda (condition)
                                     (declare (ignore condition))
                                     (or ,@variables))
                    ,@(when report
                        `(:report-function ,report)))))
         (clauses (mapcar #'process-clause clauses)))
    `(let ,all-variables
       (restart-bind ,clauses ,@body))))

;;; Printing utilities

(defun print-direction (stream direction &optional colon? at?)
  (declare (ignore colon? at?))
  (format stream "~:[-~;r~]~:[-~;w~]"
          (member direction '(:input :io))
          (member direction '(:output :io))))

(defun print-location (stream location &optional colon? at?)
  (declare (ignore colon? at?))
  (format stream "~:[N/A~;~:*~S~]"
          (typecase location
            (pathname (format nil "~A.~A"
                              (pathname-name location)
                              (pathname-type location)))
            (t        location))))
