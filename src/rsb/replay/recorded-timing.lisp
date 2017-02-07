;;;; recorded-timing.lisp ---
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; `recorded-timing' replay strategy class

(defclass recorded-timing (error-policy-mixin
                           filtering-mixin
                           timed-replay-mixin
                           delay-correcting-mixin
                           delay-limiting-mixin
                           speed-adjustment-mixin
                           timestamp-adjustment-mixin
                           print-items:print-items-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded and,
    as much as possible, with identical local temporal relations. A
    faithful replay with respect to global temporal
    relations (e.g. time between first and last event) is not
    attempted explicitly.

    Besides this default \"faithful\" replay timing, variations of the
    recorded timing can be produced as follows:

      :speed SPEED

        Scale all individual delays between events by SPEED. I.e. when
        SPEED is 0.5, all delays are doubled.

      :max-delay DELAY

        Constrain all individual delays between events to be at most
        DELAY seconds. The intention is replaying events as recorded
        when they originally occurred in quick succession but squash
        large \"gaps\" in the sequence of events into short pauses of
        DELAY seconds. "))

(service-provider:register-provider/class
 'replay-strategy :recorded-timing :class 'recorded-timing)

(defmethod schedule-event ((strategy recorded-timing)
                           (event    t)
                           (previous local-time:timestamp)
                           (next     local-time:timestamp))
  (local-time:timestamp-difference next previous))
