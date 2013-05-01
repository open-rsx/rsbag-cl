;;;; bounds-mixin.lisp --- Add index/temporal bounds to a strategy class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

;;; `bounds-mixin' mixin class

(defclass bounds-mixin ()
  ((start-index :initarg  :start-index
                :type     (or null non-negative-integer)
                :accessor %strategy-start-index
                :writer   (setf strategy-start-index) ; reader is defined below
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

(defclass time-bounds-mixin (bounds-mixin)
  ((start-time :initarg  :start-time
               :type     range-boundary/timestamp
               :accessor strategy-start-time
               :initform nil
               :documentation
               "Stores the timestamp at which the replay should start
or nil if the replay should not start at a specific time but at an
specific index or just at the first event.")
   (end-time   :initarg  :end-time
               :type     range-boundary/timestamp
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
  (let+ (((&accessors-r/o (bag connection-bag)) connection)
         ((&accessors (start-time  strategy-start-time)
                      (start-index %strategy-start-index)
                      (end-time    strategy-end-time)
                      (end-index   strategy-end-index)) strategy)
         (sequence    (make-view connection strategy
                                 :selector #'channel-timestamps))
         ((&labels timestamp->index (timestamp)
            (etypecase timestamp
              (real
               (timestamp->index
                (local-time:adjust-timestamp
                    (if (minusp timestamp) (end bag) (rsbag:start bag))
                  (:offset :sec  (floor timestamp))
                  (:offset :nsec (mod (floor timestamp 1/1000000000)
                                      1000000000)))))
              (local-time:timestamp
               (values
                (or (position timestamp sequence
                              :test #'local-time:timestamp<=)
                    (error "~@<Could not find requested timestamp ~A in bag ~
~A (with temporal range [~A, ~A]).~@:>"
                           timestamp (connection-bag connection)
                           (rsbag:start bag) (end bag)))
                timestamp)))))
         ((&flet set-index (timestamp setter name)
            (log1 :info "Mapping requested ~A ~A to index (this can take a moment)"
                  name timestamp)
            (let+ (((&values index timestamp) (timestamp->index timestamp))
                   (effective  (elt sequence index))
                   (difference (abs (local-time:timestamp-difference
                                     timestamp effective))))
              (funcall setter index)
              (log1 :info "Mapped requested ~A ~A to index ~:D (at time ~A, ~,6F seconds difference)"
                    name timestamp index effective difference)
              (when (> difference 1)
                (warn "~@<Mapped ~A ~A is rather far (~A seconds) from ~
requested ~A ~A~@:>"
                      name effective difference name timestamp))))))
    (when start-time
      (if (and (rsbag:start bag) (end bag))
          (set-index start-time
                     #'(lambda (value) (setf start-index value))
                     "start time")
          (warn "~@<Bag ~A does not have start and end times; ignoring ~
requested start time ~A~@:>"
                bag start-time)))
    (when end-time
      (if (and (rsbag:start bag) (end bag))
          (set-index end-time
                     #'(lambda (value) (setf end-index value))
                     "end time")
          (warn "~@<Bag ~A does not have start and end times; ignoring ~
requested end time ~A~@:>"
                bag end-time)))))

;;; Utility functions

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
