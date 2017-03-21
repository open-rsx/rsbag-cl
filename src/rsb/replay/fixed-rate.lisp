;;;; fixed-rate.lisp --- A strategy for replaying events at a fixed rate.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; `fixed-rate' replay strategy class

(defclass fixed-rate (error-policy-mixin
                      filtering-mixin
                      timed-replay-mixin
                      delay-correcting-mixin
                      speed-adjustment-mixin
                      timestamp-adjustment-mixin
                      print-items:print-items-mixin)
  ((delay :type     positive-real
          :accessor strategy-delay
          :documentation
          "Stores the fixed delay in seconds between publishing
           subsequent events."))
  (:documentation
   "This strategy replays events in the order they were recorded and,
    as precisely as possible, with a specified fixed rate."))

(service-provider:register-provider/class
 'replay-strategy :fixed-rate :class 'fixed-rate)

(defmethod initialize-instance :before ((instance fixed-rate)
                                        &key
                                        (delay nil delay-supplied?)
                                        (rate  nil rate-supplied?))
  (cond
    ((and (not delay-supplied?) (not rate-supplied?))
     (missing-required-initarg 'fixed-rate :delay-xor-rate))
    ((and delay-supplied? rate-supplied?)
     (incompatible-initargs 'fixed-rate :delay delay :rate rate))))

(defmethod shared-initialize :after ((instance   fixed-rate)
                                     (slot-names t)
                                     &key
                                     (delay nil delay-supplied?)
                                     (rate  nil rate-supplied?))
  (cond
    (delay-supplied? (setf (strategy-delay instance) delay))
    (rate-supplied?  (setf (strategy-rate instance) rate))))

(defmethod (setf strategy-delay) :before ((new-value real)
                                          (strategy  fixed-rate))
  (check-type new-value positive-real "a positive real number"))

(defmethod strategy-rate ((strategy fixed-rate))
  (/ (strategy-delay strategy)))

(defmethod (setf strategy-rate) ((new-value real)
                                 (strategy  fixed-rate))
  (check-type new-value positive-real "a positive real number")
  (setf (strategy-delay strategy) (/ new-value)))

(defmethod schedule-event ((strategy fixed-rate)
                           (event    t)
                           (previous local-time:timestamp)
                           (next     local-time:timestamp))
  (strategy-delay strategy))

(defmethod print-items:print-items append ((object fixed-rate))
  `((:rate ,(strategy-rate object) " ~A Hz"
           ((:after :bounding-indices)))))
