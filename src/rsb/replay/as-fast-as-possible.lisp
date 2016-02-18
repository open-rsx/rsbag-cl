;;;; as-fast-as-possible.lisp ---
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; `as-fast-as-possible' replay strategy class

(defclass as-fast-as-possible (error-policy-mixin
                               sequential-mixin
                               timestamp-adjustment-mixin)
  ()
  (:documentation
   "Replays events in the recorded order, but as fast as possible.

    Consequently, recorded timestamps are only used to establish the
    playback order of events, but not for any kind of replay
    timing."))

(service-provider:register-provider/class
 'replay-strategy :as-fast-as-possible :class 'as-fast-as-possible)
