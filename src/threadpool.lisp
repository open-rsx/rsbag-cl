;;;; threadpool.lisp --- A threadpool for async rsbag operations.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defvar *threadpool* nil
  "When non-nil, holds the threadpool used by the rsbag system.")

(defun start-threadpool ()
  "Create and initialize a threadpool for use by the rsbag system."
  (when *threadpool*
    (warn "~@<Threadpool already initialized to ~A.~@:>"
          *threadpool*))
  (setf *threadpool* (lparallel:make-kernel 2 :name "rsbag")))

(defun stop-threadpool ()
  "Stop all threads of the threadpool used by the rsbag system."
  (let ((lparallel:*kernel* *threadpool*))
    (lparallel:end-kernel :wait t))
  (setf *threadpool* nil))

(defmacro with-threadpool (&body body)
  "Execute BODY such that created tasks use the rsbag threadpool and
   errors are transferred."
  `(let ((lparallel:*kernel* *threadpool*))
     (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
       ,@body)))

;; Start the threadpool when loading or executing this. It may have to
;; be stopped and restarted when saving an image. See reloading.lisp.
(unless *threadpool*
  (start-threadpool))
