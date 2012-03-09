;;; bounds-mixin.lisp ---
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


;;; `bounds-mixin' mixin class
;;

(defclass bounds-mixin ()
  ((start-index :initarg  :start-index
		:type     (or null non-negative-integer)
		:accessor %strategy-start-index
		:accessor strategy-start-index
		:initform nil
		:documentation
		"Stores the index of the event at which the replay
should start or nil if the replay should just start at the first
event.")
   (end-index   :initarg  :end-index
		:type     (or null non-negative-integer)
		:accessor strategy-end-index
		:initform nil
		:documentation
		"Stores the index after the event at which the replay
should stop or nil if the replay should end at the final event."))
  (:documentation
   "Provides start-index and end-index slots, some consistency checks
on there values and a method on `print-object'."))

(defmethod shared-initialize :before ((instance   bounds-mixin)
				      (slot-names t)
				      &key
				      (start-index nil start-index-supplied?)
				      (end-index   nil end-index-supplied?))
  (when (and start-index-supplied? end-index-supplied?)
    (check-ordered-indices start-index end-index)))

(defmethod strategy-start-index ((strategy bounds-mixin))
  (or (%strategy-start-index strategy) 0))

(defmethod replay :before ((connection replay-bag-connection)
			   (strategy   bounds-mixin)
			   &key &allow-other-keys)
  (when-let ((start-index (%strategy-start-index strategy))
	     (end-index   (strategy-end-index    strategy)))
    (check-ordered-indices start-index end-index)))

(defmethod print-object ((object bounds-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~:D, ~:[*~;~:*~:D~]["
	    (strategy-start-index object)
	    (strategy-end-index   object))))


;;; `time-bounds-mixin' mixin class
;;

(defclass time-bounds-mixin (bounds-mixin)
  ((start-time :initarg  :start-time
	       :type     (or null local-time:timestamp)
	       :accessor strategy-start-time
	       :initform nil
	       :documentation
	       "Stores the timestamp at which the replay should start
or nil if the replay should not start at a specific time but at an
specific index or just at the first event.")
   (end-time   :initarg  :end-time
	       :type     (or null local-time:timestamp)
	       :accessor strategy-end-time
	       :initform nil
	       :documentation
	       "Stores the timestamp at which the replay should stop
or nil if the replay should not stop at a specific time."))
  (:documentation
   "This mixin class adds start-time and end-time slots and
translation of their values into indices before replay."))

(defmethod shared-initialize :before ((instance   time-bounds-mixin)
				      (slot-names t)
				      &key
				      (start-index nil start-index-supplied?)
				      (end-index   nil end-index-supplied?)
				      (start-time  nil start-time-supplied?)
				      (end-time    nil end-time-supplied?))
  (declare (ignore start-index end-index))

  (when (and start-index-supplied? start-time-supplied?)
    (error "~@<The initargs ~S and ~S are mutually exclusive.~@:>"
	   :start-index :start-time))
  (when (and end-index-supplied? end-time-supplied?)
    (error "~@<The initargs ~S and ~S are mutually exclusive.~@:>"
	   :end-index :end-time))

  (when (and start-time-supplied? end-time-supplied?)
    (check-ordered-timestamps start-time end-time)))

(defmethod replay :before ((connection replay-bag-connection)
			   (strategy   time-bounds-mixin)
			   &key &allow-other-keys)
  (bind (((:accessors-r/o (bag connection-bag)) connection)
	 ((:accessors (start-time  strategy-start-time)
		      (start-index %strategy-start-index)
		      (end-time    strategy-end-time)
		      (end-index   strategy-end-index)) strategy)
	 (sequence    (make-view connection strategy
				 :selector #'channel-timestamps))
	 ((:flet timestamp->index (timestamp))
	  (or (position timestamp sequence
			:test #'local-time:timestamp<=)
	      (error "~@<Could not find requested timestamp ~A in bag ~
~A (with temporal range [~A, ~A]).~@:>"
		     timestamp (connection-bag connection)
		     (rsbag:start bag) (end bag))))
	 ((:flet set-index (timestamp setter name))
	  (let* ((index      (timestamp->index timestamp))
		 (effective  (elt sequence index))
		 (difference (abs (local-time:timestamp-difference
				   timestamp effective))))
	    (funcall setter index)
	    (log1 :info "Mapped requested ~A ~A to index ~:D (at time ~A, ~,6F seconds difference)"
		  name timestamp index effective difference)
	    (when (> difference 1)
	      (warn "~@<Mapped ~A ~A is rather far (~A seconds) from ~
requested ~A ~A~@:>"
		    name effective difference name timestamp)))))
	(when start-time
	  (set-index start-time
		     #'(lambda (value) (setf start-index value))
		     "start time"))
	(when end-time
	  (set-index end-time
		     #'(lambda (value) (setf end-index value))
		     "end time"))))


;;; Utility functions
;;

(defun check-ordered-indices (earlier later)
  "Signal an error unless EARLIER is smaller than LATER."
  (unless (< earlier later)
    (error "~@<Invalid relation of indices: index ~:D is not greater ~
than index ~:D.~@:>" later earlier)))

(defun check-ordered-timestamps (earlier later)
  "Signal an error unless EARLIER is earlier than LATER."
  (unless (local-time:timestamp< earlier later)
    (error "~@<Invalid relation of timestamps: timestamp ~A is not ~
later than than timestamp ~A.~@:>" later earlier)))
