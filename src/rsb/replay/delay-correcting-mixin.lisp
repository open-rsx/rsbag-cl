;;; delay-correcting-mixin.lisp --- A mixin class for correcting delays.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsbag.rsb.replay)

(defclass delay-correcting-mixin ()
  ((previous-delay :type     (or null non-negative-real)
		   :accessor strategy-previous-delay
		   :initform nil
		   :documentation
		   "Stores the previously scheduled delay to estimate
the difference between the scheduled and actual delay.")
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
  (bind (((:accessors (previous-delay strategy-previous-delay)
		      (previous-call  strategy-previous-call)) strategy)
	 (now          (local-time:now))
	 (actual-delay (when previous-call
			 (local-time:timestamp-difference
			  now previous-call)))
	 (overshoot    (if (and actual-delay previous-delay )
			   (- actual-delay previous-delay)
			   0))
	 (corrected    (- (call-next-method) overshoot)))
    (setf previous-delay corrected
	  previous-call     now)
    corrected))
