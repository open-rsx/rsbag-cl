;;;; delay-correcting-mixin.lisp --- A mixin class for correcting delays.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

(defclass delay-correcting-mixin ()
  ((previous-delay :type     (or null real)
                   :accessor strategy-previous-delay
                   :initform nil
                   :documentation
                   "Stores the previously scheduled delay to estimate
the difference between the scheduled and actual delay. This can become
negative when the previous wait executed too slowly.")
   (previous-call  :type     (or null local-time:timestamp)
                   :accessor strategy-previous-call
                   :initform nil
                   :documentation
                   "Stores a timestamp for the previous call to
`schedule-event' to estimate how much time actually (as opposed to the
scheduled time) passed between the previous and the current call."))
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that compute an ideal delay between successive events and need to have
this delay adjusted to compensate for processing latencies."))

(defmethod schedule-event :around ((strategy delay-correcting-mixin)
                                   (event    t)
                                   (previous local-time:timestamp)
                                   (next     local-time:timestamp))
  ;; Compute the difference between the previously scheduled duration
  ;; and the actual duration. Adjust the next scheduled duration
  ;; accordingly.
  ;;
  ;; CORRECTED can become negative if at the same time
  ;; 1) the planned delay (returned by (call-next-method) is very
  ;;    small
  ;; 2) OVERSHOOT is positive
  ;; When this happens, no wait is performed and the negative
  ;; PREVIOUS-DELAY hopefully leads to a compensation in the next
  ;; call.
  (let+ (((&accessors (previous-delay strategy-previous-delay)
                      (previous-call  strategy-previous-call)) strategy)
         (now          (local-time:now))
         (actual-delay (when previous-call
                         (local-time:timestamp-difference
                          now previous-call)))
         (overshoot    (if (and actual-delay previous-delay)
                           (- actual-delay previous-delay)
                           0))
         (corrected    (- (call-next-method) overshoot)))
    (setf previous-delay corrected
          previous-call  now)
    corrected))
