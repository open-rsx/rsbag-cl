;;; fixed-rate.lisp ---
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


;;; `fixed-rate' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :fixed-rate)))
  (find-class 'fixed-rate))

(defclass fixed-rate (error-policy-mixin
		      timed-replay-mixin
		      delay-correcting-mixin
		      speed-adjustment-mixin
		      timestamp-adjustment-mixin)
  ((delay :initarg  :delay
	  :type     positive-real
	  :accessor strategy-delay
	  :initform .1
	  :documentation
	  "Stores the fixed delay in seconds between publishing
subsequent events."))
  (:documentation
   "This strategy replays events in the order they were recorded and,
as precisely as possible, with a specified fixed rate."))

(defmethod shared-initialize :before ((instance   fixed-rate)
				      (slot-names t)
				      &key
					delay
					rate)
  (cond
    ((and (null delay) (null rate))
     (required-argument :delay-or-rate))
    ((and delay rate)
     (error "~@<The initargs ~S and ~S are mutually exclusive.~@:>"
	    :delay :rate))))

(defmethod shared-initialize :after ((instance   fixed-rate)
                                     (slot-names t)
                                     &key
				       rate)
  (when rate
    (setf (strategy-rate instance) rate)))

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

(defmethod print-object ((object fixed-rate) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A Hz" (strategy-rate object))))
