;;;; timed-replay-mixin.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

(defclass timed-replay-mixin (sequential-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy
classes perform time-based scheduling of replayed events."))

(defmethod process-event :before ((connection         replay-bag-connection)
				  (strategy           timed-replay-mixin)
				  (timestamp          local-time:timestamp)
				  (previous-timestamp local-time:timestamp)
				  (event              t)
				  (sink               t))
  "Delay the publishing of EVENT for the amount of time computed by
`schedule-event'."
  (let ((amount (schedule-event strategy event previous-timestamp timestamp)))
    (when (plusp amount)
      (sleep amount))))
