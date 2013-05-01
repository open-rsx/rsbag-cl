;;;; protocol.lisp --- Protocol functions of the rsb.replay module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

;;; Bounds protocol

(defgeneric strategy-start-index (strategy)
  (:documentation
   "Return the start index of the region processed by STRATEGY."))

(defgeneric strategy-end-index (strategy)
  (:documentation
   "Return the end index of the region processed by STRATEGY."))

;;; Fixed-rate strategy protocol

(defgeneric strategy-delay (strategy)
  (:documentation
   "Return the delay in seconds between events replayed with
STRATEGY."))

(defgeneric (setf strategy-delay) (new-value strategy)
  (:documentation
   "Set the delay in seconds between events replayed with STRATEGY to
NEW-VALUE."))

(defgeneric strategy-rate (strategy)
  (:documentation
   "Return the rate in Hertz of events replayed with STRATEGY."))

(defgeneric (setf strategy-rate) (new-value strategy)
  (:documentation
   "Set the rate in Hertz of events replayed with STRATEGY to
NEW-VALUE."))
