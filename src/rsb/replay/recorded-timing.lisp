;;;; recorded-timing.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

;;; `recorded-timing' replay strategy class

(defmethod find-replay-strategy-class ((spec (eql :recorded-timing)))
  (find-class 'recorded-timing))

(defclass recorded-timing (error-policy-mixin
                           timed-replay-mixin
                           delay-correcting-mixin
                           speed-adjustment-mixin
                           timestamp-adjustment-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded and,
as much as possible, with identical local temporal relations. A
faithful replay with respect to global temporal relations (e.g. time
between first and last event) is not attempted explicitly."))

(defmethod schedule-event ((strategy recorded-timing)
                           (event    t)
                           (previous local-time:timestamp)
                           (next     local-time:timestamp))
  (local-time:timestamp-difference next previous))
