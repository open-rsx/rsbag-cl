;;;; reloading.lisp --- Support for image dumping and reloading.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defun enable-restart-threadpool ()
  "Make sure that the rsbag threadpool is shutdown when saving a core
and restarted when loading a core."
  #+sbcl (progn
           (pushnew 'stop-threadpool  sb-ext:*save-hooks*)
           (pushnew 'start-threadpool sb-ext:*init-hooks*))
  #-sbcl (error "Restarting the rsbag threadpool is not supported in this Lisp."))
