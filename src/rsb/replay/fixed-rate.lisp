;;; fixed-rate.lisp ---
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
