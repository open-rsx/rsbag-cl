;;;; macros.lisp --- Convenience macros provided by the cl-rsbag system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag)

(defmacro with-bag ((var source
		     &rest args
		     &key &allow-other-keys)
		    &body body)
  "Execute BODY with VAR bound to a bag object for the data source
SOURCE. ARGS are passed to `open-bag'."
  (check-type var symbol "a symbol")

  `(let ((,var (open-bag ,source ,@args)))
     (unwind-protect
	  (progn ,@body)
       (close ,var))))
