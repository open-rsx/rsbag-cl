;;;; strategy-mixin.lisp --- Mixins classes for replay strategy classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; `error-policy-mixin' mixin class

(defclass error-policy-mixin (rsb.ep:error-policy-mixin)
  ()
  (:default-initargs
   :error-policy #'log-error)
  (:documentation
   "This mixin class provides a method on `replay' that arranges for
the next `replay' methods to be called with error handling based on
the installed error policy."))

(defmethod replay :around ((connection replay-bag-connection)
                           (strategy   error-policy-mixin)
                           &key &allow-other-keys)
  (rsb.ep:with-error-policy (strategy)
    (call-next-method)))

;;; `replay-restart-mixin' mixin class

(defclass replay-restart-mixin ()
  ()
  (:documentation
   "This mixin class add the establishing of continue and log restarts
around the actual work of the `replay' method."))

(defmethod replay :around ((connection replay-bag-connection)
                           (strategy   replay-restart-mixin)
                           &key &allow-other-keys)
  (handler-bind
      ((error (lambda (condition)
                (restart-case
                    (error 'event-retrieval-failed
                           :connection connection
                           :strategy   strategy
                           :cause      condition)
                  (continue (&optional condition)
                    :report (lambda (stream)
                              (format stream "~@<Ignore the failed ~
                                              event and continue ~
                                              with the next ~
                                              event.~@:>"))
                    (declare (ignore condition))
                    (use-value :skip))
                  (log (&optional condition)
                    :report (lambda (stream)
                              (format stream "~@<Log an error ~
                                              message and continue ~
                                              with the next ~
                                              event.~@:>"))
                    (log1 :error "Failed to retrieve an event for replay: ~A"
                          condition)
                    (use-value :skip))))))
    (call-next-method)))

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
                     (lambda (value) (setf start-index value))
                     "start time")
          (warn "~@<Bag ~A does not have start and end times; ignoring ~
                 requested start time ~A~@:>"
                bag start-time)))
    (when end-time
      (if (and (rsbag:start bag) (end bag))
          (set-index end-time
                     (lambda (value) (setf end-index value))
                     "end time")
          (warn "~@<Bag ~A does not have start and end times; ignoring ~
                 requested end time ~A~@:>"
                bag end-time)))))

;;; `view-creation-mixin' mixin class

(defclass view-creation-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that have to construct a view sequence for multiple channels. The
generic function `make-view' can be used to customize this
behavior. The method for `view-creation-mixin' creates a
serialized view of events across channels."))

(defmethod make-view ((connection replay-bag-connection)
                      (strategy   view-creation-mixin)
                      &key
                      (selector (rcurry #'inject-informer connection)))
  "Default behavior is serializing events across channels."
  (make-serialized-view
   (mappend #'connection-channels (connection-channels connection))
   :selector selector))

;;; `sequential-mixin' mixin class

(defclass sequential-mixin (replay-restart-mixin
                            time-bounds-mixin
                            view-creation-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that essentially process all events in a sequential manner. The method
on `replay' for `sequential-mixin' creates a sequence via `make-view'
and processes all elements of the sequence by sequential calls to
`process-event'."))

(defmethod replay ((connection replay-bag-connection)
                   (strategy   sequential-mixin)
                   &key
                   progress)
  (let+ (((&accessors-r/o (start-index strategy-start-index)
                          (end-index   strategy-end-index)) strategy)
         (sequence        (make-view connection strategy))
         (update-progress (%make-progress-reporter sequence progress)))
    (macrolet
        ((do-it (&optional end-index)
           `(iter (for (timestamp event sink) each sequence
                       :from start-index
                       ,@(when end-index '(:below end-index)))
                  (for previous-timestamp previous timestamp)
                  (for i :from start-index)
                  (process-event connection strategy
                                 timestamp previous-timestamp
                                 event sink)
                  (when update-progress
                    (funcall update-progress i timestamp)))))
      (if end-index
          (do-it end-index)
          (do-it)))))

(defmethod process-event ((connection         replay-bag-connection)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              (eql :skip))
                          (sink               t))
  "Error recovery behaviors may inject the value :skip for EVENT. The
default behavior is just ignoring the failed event. "
  (values))

(defmethod process-event ((connection         replay-bag-connection)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              t)
                          (sink               t))
  "The default behavior consists in sending EVENT via SINK which is
assumed to be an `rsb:informer'."
  (send sink event :unchecked? t))

(defmethod process-event ((connection         replay-bag-connection)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              t)
                          (sink               function))
  "The default behavior for a function SINK consists in calling SINK
with EVENT."
  (funcall sink event))

;;; `timed-replay-mixin' mixin class

(defclass timed-replay-mixin (sequential-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy
classes perform time-based scheduling of replayed events."))

(defmethod process-event :before ((connection         replay-bag-connection)
                                  (strategy           timed-replay-mixin)
                                  (timestamp          local-time:timestamp)
                                  (previous-timestamp local-time:timestamp)
                                  (event              t)
                                  (sink               t))
  "Delay the publishing of EVENT for the amount of time computed by
`schedule-event'."
  (let ((amount (schedule-event strategy event previous-timestamp timestamp)))
    (when (plusp amount)
      (sleep amount))))

;;; `delay-correcting-mixin' mixin class

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

;;; `speed-adjustment-mixin' mixin class

(defclass speed-adjustment-mixin ()
  ((speed :initarg  :speed
          :type     positive-real
          :accessor strategy-speed
          :initform 1
          :documentation
          "Stores the speed factor that should be applied to the
results of scheduling events."))
  (:documentation
   "This mixin class adds to timed replay strategy classes the ability
to speed up or slow down replay speed by a constant factor."))

(defmethod schedule-event :around ((strategy speed-adjustment-mixin)
                                   (event    t)
                                   (previous local-time:timestamp)
                                   (next     local-time:timestamp))
  (/ (call-next-method) (strategy-speed strategy)))

;;; `timestamp-adjustment-mixin' mixin class

(defclass timestamp-adjustment-mixin ()
  ((adjustments :type     list
                :accessor strategy-adjustments
                :initform nil
                :documentation
                "Stores a list of adjustments of the form

  (TIMESTAMP NEW-VALUE)

where TIMESTAMP is a keyword designating a timestamp and NEW-VALUE
specifies the new value. Currently, NEW-VALUE can be the symbol :NOW
or a `local-time:timestamp' object."))
  (:documentation
   "This mixin class adds the ability to adjust event timestamps
during replay."))

(defmethod shared-initialize :after ((instance   timestamp-adjustment-mixin)
                                     (slot-names t)
                                     &key
                                     (adjustments nil adjustments-supplied?))
  (when adjustments-supplied?
    (setf (strategy-adjustments instance) adjustments)))

(defmethod (setf strategy-adjustments) :before ((new-value t)
                                                (object    timestamp-adjustment-mixin))
  (check-type new-value list "a list of timestamp adjustment specifications")
  (iter (for spec in new-value)
        (check-type spec timestamp-adjustment-spec)))

(defmethod process-event :before ((connection         replay-bag-connection)
                                  (strategy           timestamp-adjustment-mixin)
                                  (timestamp          t)
                                  (previous-timestamp t)
                                  (event              event)
                                  (sink               t))
  "The default behavior consists in sending EVENT via INFORMER."
  (iter (for (key value) in (strategy-adjustments strategy))
        (setf (timestamp event key)
              (etypecase value
                (timestamp-adjustment-value/now
                 (local-time:now))

                (timestamp-adjustment-value/copy
                 (let ((key (second value)))
                   (or (timestamp event key)
                       (error "~@<Event ~A does not have a ~A ~
                               timestamp.~:@>"
                              event key))))

                (timestamp-adjustment-value/delta
                 (let+ (((&values sec nsec) (floor (second value))))
                   (local-time:adjust-timestamp (timestamp event key)
                     (:offset :sec  sec)
                     (:offset :nsec (floor (* 1000000000 nsec))))))

                (local-time:timestamp
                 value)))))

(defmethod print-object ((object timestamp-adjustment-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[~A~]"
            (mapcar #'first (strategy-adjustments object)))))

;;; `external-driver-mixin' mixin class

(defclass external-driver-mixin (sequential-mixin)
  ((commands :type     list
             :reader   strategy-commands
             :accessor %strategy-commands
             :initform nil
             :documentation
             "Stores available commands as an alist of items of the
form (NAME . IMPLEMENTING-FUNCTION)."))
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that depend on some kind of external driver to control iteration
through the sequential data. Instances store a list of available
commands which are available for invocation by the external driver
mechanism. The `replay' method basically retrieves subsequent commands
and executes them until termination is requested."))

(defmethod find-command ((strategy external-driver-mixin)
                         (name     string)
                         &key
                         (test   #'string=)
                         (error? t))
  (or (cdr (assoc name (strategy-commands strategy)
                  :test test
                  :key  #'symbol-name))
      (when error?
        (error "~@<No such command: ~S.~@:>" name))))

(defmethod make-commands ((strategy external-driver-mixin)
                          (sequence sequence)
                          &key
                          (length    (missing-required-argument :length))
                          (step      (missing-required-argument :step))
                          (index     (missing-required-argument :index))
                          (element   (missing-required-argument :element))
                          (emit      (missing-required-argument :emit))
                          (terminate (missing-required-argument :terminate)))
  "Return a default alist of commands."
  `(;; Queries
    (:length         . ,(lambda ()
                          (funcall length)))
    (:relativelength . ,(lambda ()
                          (funcall length t)))
    (:index          . ,(lambda ()
                          (funcall index)))
    (:relativeindex  . ,(lambda ()
                          (funcall index t)))
    (:time           . ,(lambda ()
                          (princ-to-string (first (funcall element)))))
    ;; Position
    (:next           . ,(lambda ()
                          (funcall step nil)
                          (funcall index)))
    (:previous       . ,(lambda ()
                          (funcall step t)
                          (funcall index)))
    (:seek           . ,(lambda (position)
                          (let ((diff (- position (funcall index))))
                            (iter (repeat (abs diff))
                                  (funcall step (minusp diff))))
                          (values)))
    ;; Emission
    (:emit           . ,(lambda ()
                          (funcall emit)
                          (values)))
    (:emitandnext    . ,(lambda ()
                          (funcall emit)
                          (funcall step nil)
                          (funcall index)))

    (:get            . ,(lambda ()
                          (event-data (second (funcall element)))))

    ;; Session
    (:quit           . ,(lambda ()
                          (funcall terminate)
                          (values)))))

(defmethod execute-command ((strategy external-driver-mixin)
                            (command  function))
  (funcall command))

(defmethod replay ((connection replay-bag-connection)
                   (strategy   external-driver-mixin)
                   &key
                   progress)
  (let+ (((&accessors-r/o (start-index strategy-start-index)
                          (end-index   strategy-end-index)) strategy)
         (sequence        (make-view connection strategy))
         (update-progress (%make-progress-reporter sequence progress))
         terminate?
         ;; Iteration state.
         ((&values current limit from-end?)
          (sequence:make-simple-sequence-iterator
           sequence :start start-index :end end-index))
         (previous-timestamp)
         ;; Primitive state query and manipulation functions.
         ((&labels length* (&optional relative-to-bounds?)
            (if relative-to-bounds?
                (- (or end-index (length*)) start-index)
                (length sequence))))
         ((&flet end? (back?)
            (sequence:iterator-endp
             sequence current
             (if back? (1- start-index) limit) (xor back? from-end?))))
         ((&flet step* (back?)
            (setf current (sequence:iterator-step
                           sequence current (xor back? from-end?)))
            (when (end? back?)
              (setf current (sequence:iterator-step
                             sequence current (xor (not back?) from-end?)))
              (error "~@<Attempt to step beyond ~:[end~;beginning~] of ~
                      sequence. Current position ~:D, valid range ~
                      [~:D, ~:D[.~@:>"
                     back? (sequence:iterator-index sequence current)
                     start-index end-index))))
         ((&labels index (&optional relative-to-bounds?)
            (if relative-to-bounds?
                (- (index) start-index)
                (sequence:iterator-index sequence current))))
         ((&flet element ()
            (sequence:iterator-element sequence current)))
         ((&flet emit ()
            (let+ (((timestamp event sink) (element)))
              (process-event connection strategy
                             timestamp previous-timestamp
                             event sink)
              (setf previous-timestamp timestamp))))
         ((&flet terminate ()
            (setf terminate? t))))

    (setf (%strategy-commands strategy)
          (make-commands strategy sequence
                         :length    #'length*
                         :step      #'step*
                         :index     #'index
                         :element   #'element
                         :emit      #'emit
                         :terminate #'terminate))

    (iter (until terminate?)
          (for command next (next-command strategy))
          (execute-command strategy command)
          (when update-progress
            (funcall update-progress (index) (first (element)))))))

;;; Utility functions

(defun check-ordered-indices (earlier later)
  "Signal an error unless EARLIER is smaller than LATER."
  (unless (< earlier later)
    (error "~@<Invalid relation of indices: index ~:D is not greater ~
            than index ~:D.~@:>"
           later earlier)))

(defun check-ordered-timestamps (earlier later)
  "Signal an error unless EARLIER is earlier than LATER."
  (unless (local-time:timestamp< earlier later)
    (error "~@<Invalid relation of timestamps: timestamp ~A is not ~
            later than timestamp ~A.~@:>"
           later earlier)))
