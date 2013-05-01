;;;; as-fast-as-possible.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

;;; `as-fast-as-possible' replay strategy class

(defmethod find-replay-strategy-class ((spec (eql :as-fast-as-possible)))
  (find-class 'as-fast-as-possible))

(defclass as-fast-as-possible (error-policy-mixin
                               sequential-mixin
                               timestamp-adjustment-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded, but
as fast as possible. Consequently, recorded timestamps are only used
to establish the playback order of events, but not for any kind of
replay timing."))
