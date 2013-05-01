;;;; speed-adjustment-mixin.lisp --- Mixin that scales scheduled playback times.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

(defclass speed-adjustment-mixin ()
  ((speed :initarg  :speed
	  :type     positive-real
	  :accessor strategy-speed
	  :initform 1
	  :documentation
	  "Stores the speed factor that should be applied to the
results of scheduling events."))
  (:documentation
   "This mixin class adds to timed replay strategy classes the ability
to speed up or slow down replay speed by a constant factor."))

(defmethod schedule-event :around ((strategy speed-adjustment-mixin)
				   (event    t)
				   (previous local-time:timestamp)
				   (next     local-time:timestamp))
  (/ (call-next-method) (strategy-speed strategy)))
