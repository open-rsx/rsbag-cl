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
		   (strategy   timed-replay-mixin))
  (bind (((:accessors-r/o (start-index strategy-start-index)
			  (end-index   strategy-end-index)) strategy)
	 (sequence (make-serialized-view
		    (map 'list #'connection-channel
			 (connection-channels connection))
		    :selector (rcurry #'inject-informer connection))))
    (iter (for (timestamp event informer) each     sequence
	       :from start-index
	       :to   end-index)
	  (for previous-timestamp         previous timestamp)
	  (sleep (schedule-event strategy event previous-timestamp timestamp))
	  (rsb:send informer event))))


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
				  :key #'connection-channel))))
