;;;; error-policy-mixin.lisp --- error-policy-mixin mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

(defclass error-policy-mixin (rsb.ep:error-policy-mixin)
  ()
  (:default-initargs
   :error-policy #'log-error)
  (:documentation
   "This mixin class provides a method on `replay' that arranges for
the next `replay' methods to be called with error handling based on
the installed error policy."))

(defmethod replay :around ((connection replay-bag-connection)
			   (strategy   error-policy-mixin)
			   &key &allow-other-keys)
  (rsb.ep:with-error-policy (strategy)
    (call-next-method)))
