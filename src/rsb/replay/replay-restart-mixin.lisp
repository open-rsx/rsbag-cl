;;;; replay-restart-mixin.lisp --- replay-restart-mixin mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

(defclass replay-restart-mixin ()
  ()
  (:documentation
   "This mixin class add the establishing of continue and log restarts
around the actual work of the `replay' method."))

(defmethod replay :around ((connection replay-bag-connection)
			   (strategy   replay-restart-mixin)
			   &key &allow-other-keys)
  (handler-bind
      ((error #'(lambda (condition)
		  (restart-case
		      (error 'event-retrieval-failed
			     :connection connection
			     :strategy   strategy
			     :cause      condition)
		    (continue (&optional condition)
		      :report (lambda (stream)
				(format stream "~@<Ignore the ~
failed event and continue with the next event.~@:>"))
                      (declare (ignore condition))
		      (use-value :skip))
		    (log (&optional condition)
		      :report (lambda (stream)
				(format stream "~@<Log an error ~
message and continue with the next event.~@:>"))
		      (log1 :error "Failed to retrieve an event for replay: ~A"
			    condition)
		      (use-value :skip))))))
    (call-next-method)))
