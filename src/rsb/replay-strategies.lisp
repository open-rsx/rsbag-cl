;;; replay-strategies.lisp --- Class implementing replay strategies.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsbag.rsb)


;;; `bounds-mixin' mixin class
;;

(defclass bounds-mixin ()
  ((start-index :initarg  :start-index
		:type     non-negative-integer
		:accessor strategy-start-index
		:initform 0
		:documentation
		"Stores the index of the event at which the replay
should start.")
   (end-index   :initarg  :end-index
		:type     (or null non-negative-integer)
		:accessor strategy-end-index
		:initform nil
		:documentation
		"Stores the index after the event at which the replay
should stop or nil if the replay should end at the final event of the
recording."))
  (:documentation
   "Provides start-index and end-index slots and a method on
`print-object'."))

(defmethod print-object ((object bounds-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~D, ~:[*~;~D~]["
	    (strategy-start-index object)
	    (strategy-end-index   object))))


;;; `timed-replay-mixin' mixin class
;;

(defclass timed-replay-mixin (bounds-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy
classes perform time-based scheduling of replayed events."))

(defmethod replay ((connection replay-bag-connection)
		   (strategy   timed-replay-mixin)
		   &key
		   progress)
  (bind (((:accessors-r/o (start-index strategy-start-index)
			  (end-index   strategy-end-index)) strategy)
	 (sequence (make-serialized-view
		    (mappend #'connection-channels
			     (connection-channels connection))
		    :selector (rcurry #'inject-informer connection)))
	 (update-progress (%make-progress-reporter sequence progress)))
    (iter (for (timestamp event informer) each     sequence
	       :from start-index
	       :to   end-index)
	  (for previous-timestamp         previous timestamp)
	  (for i :from start-index)
	  (sleep (schedule-event strategy event previous-timestamp timestamp))
	  (rsb:send informer event)
	  (funcall update-progress i timestamp))))


;;; `recorded-timing' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :recorded-timing)))
  (find-class 'recorded-timing))

(defclass recorded-timing (timed-replay-mixin)
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
  (* 0.95 (local-time:timestamp-difference next previous)))


;;; `fixed-rate' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :fixed-rate)))
  (find-class 'fixed-rate))

(defclass fixed-rate (timed-replay-mixin)
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


;;; `as-fast-as-possible' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :as-fast-as-possible)))
  (find-class 'as-fast-as-possible))

(defclass as-fast-as-possible (bounds-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded, but
as fast as possible. Consequently, recorded timestamps are only used
to establish the playback order of events, but not for any kind of
replay timing."))

;;; TODO(jmoringe): avoid duplication; see timed-replay-mixin
(defmethod replay ((connection replay-bag-connection)
		   (strategy   as-fast-as-possible)
		   &key
		   progress)
  (bind (((:accessors-r/o (start-index strategy-start-index)
			  (end-index   strategy-end-index)) strategy)
	 (sequence (make-serialized-view
		    (mappend #'connection-channels
			     (connection-channels connection))
		    :selector (rcurry #'inject-informer connection)))
	 (update-progress (%make-progress-reporter sequence progress)))
    (iter (for (timestamp event informer) each sequence
	       :from start-index
	       :to   end-index)
	  (for i :from start-index)
	  (rsb:send informer event)
	  (funcall update-progress i timestamp))))


;;; `informer-injector' helper class
;;

(defclass informer-injector (channel-items)
  ((informer :initarg  :informer
	     :reader   %informer-injector-informer
	     :documentation
	     "Stores the informer that should be associated with the
channel."))
  (:documentation
   "Instance of this helper class inject a given object (usually an
`rsb:informer' instance) into each element of the underlying
sequence."))

(defmethod sequence:elt ((sequence informer-injector)
			 (index    integer))
  (append (call-next-method)
	  (list (%informer-injector-informer sequence))))

(defun inject-informer (channel connection)
  ;; Find the channel-connection for CHANNEL in CONNECTION, extract
  ;; the informer and pass it to a new `informer-injector' instance.
  (make-instance 'informer-injector
		 :channel  channel
		 :informer (connection-participant
			    (find channel (connection-channels connection)
				  :test #'member
				  :key  #'connection-channels))))


;;; Utility functions
;;

(defun %make-progress-reporter (sequence callback)
  "Return a function with two parameters that calls CALLBACK in the appropriate way if CALLBACK is non-nil"
  (if callback
      (bind (((start end) (list 0 (1- (length sequence)))))
	#'(lambda (index timestamp)
	    (funcall callback
		     (/ (- index start) (- end start))
		     index start end
		     timestamp)))
      #'(lambda (index timestamp)
	  (declare (ignore index timestamp)))))
