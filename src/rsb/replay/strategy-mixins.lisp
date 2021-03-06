;;;; strategy-mixin.lisp --- Mixins classes for replay strategy classes.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; `error-policy-mixin' mixin class

(defclass error-policy-mixin (rsb.ep:error-policy-mixin)
  ()
  (:documentation
   "This mixin class provides a method on `replay' that arranges for
    the next `replay' methods to be called with error handling based
    on the installed error policy."))

(defmethod replay :around ((connection t)
                           (strategy   error-policy-mixin)
                           &key &allow-other-keys)
  (rsb.ep:with-error-policy (strategy)
    (call-next-method)))

;;; `replay-restart-mixin' mixin class

(defclass replay-restart-mixin ()
  ()
  (:documentation
   "This mixin class adds the establishing of a `continue' and an
    `abort' restart around the actual work of the `replay' method."))

(defvar *skip* nil)

(defmethod replay :around ((connection t)
                           (strategy   replay-restart-mixin)
                           &key &allow-other-keys)
  (function-calling-restart-bind
      (((continue (&optional condition) *skip* bail)
        :report (lambda (stream)
                  (format stream
                          (if *skip*
                              "~@<Ignore the failed event and ~
                                 continue with the next event.~@:>"
                              "~@<Stop replaying~@:>"))))
       ((abort (&optional condition) bail)
        :report (lambda (stream)
                  (format stream "~@<Stop replaying~@:>"))))
    (setf bail (lambda (&optional condition)
                 (declare (ignore condition))
                 (return-from replay)))
    (call-next-method)))

;;; `bounds-mixin' mixin class

(defclass bounds-mixin ()
  ((start-index :initarg  :start-index
                :type     (or null integer)
                :accessor strategy-%start-index
                :writer   (setf strategy-start-index) ; reader is defined below
                :initform nil
                :documentation
                "Stores the index of the event at which the replay
                 should start or nil if the replay should just start
                 at the first event.")
   (end-index   :initarg  :end-index
                :type     (or null integer)
                :accessor strategy-end-index
                :initform nil
                :documentation
                "Stores the index after the event at which the replay
                 should stop or nil if the replay should end at the
                 final event."))
  (:documentation
   "Provides start-index and end-index slots, some consistency checks
    on there values and a method on `print-items'."))

(defmethod shared-initialize :before ((instance   bounds-mixin)
                                      (slot-names t)
                                      &key
                                      (start-index nil start-index-supplied?)
                                      (end-index   nil end-index-supplied?))
  (when start-index-supplied?
    (check-type start-index (or null integer)))
  (when end-index-supplied?
    (check-type end-index (or null integer)))

  (when (and start-index-supplied? end-index-supplied?)
    (with-condition-translation
        (((error incompatible-initargs)
          :class      'bounds-mixin
          :parameters (list :start-index :end-index)
          :values     (list start-index  end-index)))
      (check-ordered-indices start-index end-index))))

(defmethod strategy-start-index ((strategy bounds-mixin))
  (or (strategy-%start-index strategy) 0))

(defmethod replay :before ((connection t)
                           (strategy   bounds-mixin)
                           &key &allow-other-keys)
  (let+ (((&structure strategy- (start-index %start-index) end-index) strategy)
         (length)
         ((&flet length1 ()
            (or length
                (setf length (length (make-view
                                      connection strategy
                                      :selector #'channel-timestamps))))))
         ((&flet from-end (index name)
            (when (> (abs index) (length1))
              (error "~@<Requested ~A, ~:D elements from the end, is ~
                      not within the bounds [~:D, ~:D[.~@:>"
                     name (abs index) 0 (length1)))
            (+ (length1) index)))
         ((&flet check-index (index name)
            (when (and index (not (<= 0 index (length1))))
              (error "~@<Requested ~A ~:D is not within the bounds ~
                      [~:D, ~:D[.~@:>"
                     name index 0 (length1))))))
    (when (and start-index (minusp start-index))
      (setf start-index (from-end start-index "start index")))
    (when (and end-index (minusp end-index))
      (setf end-index (from-end end-index "end index")))
    (check-index start-index "start index")
    (check-index end-index "end index")
    (when (and start-index end-index)
      (check-ordered-indices start-index end-index))))

(defmethod print-items:print-items append ((object bounds-mixin))
  (let+ (((&structure-r/o strategy- start-index end-index) object))
    `((:bounding-indices ,(list start-index end-index) "~{[~:D, ~:[*~;~:*~:D~][~}"))))

;;; `time-bounds-mixin' mixin class

(defclass time-bounds-mixin (bounds-mixin)
  ((start-time :initarg  :start-time
               :type     range-boundary/timestamp
               :reader   strategy-start-time
               :accessor strategy-%start-time
               :initform nil
               :documentation
               "Stores the timestamp at which the replay should start
                or nil if the replay should not start at a specific
                time but at an specific index or just at the first
                event.")
   (end-time   :initarg  :end-time
               :type     range-boundary/timestamp
               :reader   strategy-end-time
               :accessor strategy-%end-time
               :initform nil
               :documentation
               "Stores the timestamp at which the replay should stop
                or nil if the replay should not stop at a specific
                time."))
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
  (when (and start-index-supplied? start-time-supplied?)
    (incompatible-initargs 'time-bounds-mixin
                           :start-index start-index
                           :start-time  start-time))
  (when start-time-supplied?
    (check-type start-time range-boundary/timestamp))

  (when (and end-index-supplied? end-time-supplied?)
    (incompatible-initargs 'time-bounds-mixin
                           :end-index end-index
                           :end-time  end-time))
  (when end-time-supplied?
    (check-type end-time range-boundary/timestamp))

  ;; This check may do nothing when both times are supplied but one is
  ;; a negative real and one is a non-negative real or when one is a
  ;; real and one is a timestamp. Therefore, the times are checked
  ;; again in the :before method on `replay'.
  (when (and start-time-supplied? end-time-supplied?)
    (with-condition-translation
        (((error incompatible-initargs)
          :class      'time-bounds-mixin
          :parameters (list :start-time :end-time)
          :values     (list start-time  end-time)))
      (check-ordered-times start-time end-time))))

(defmethod replay :before ((connection t)
                           (strategy   time-bounds-mixin)
                           &key &allow-other-keys)
  (let+ (((&structure strategy-
                      (start-time %start-time) start-index
                      (end-time   %end-time)   end-index)
          strategy)
         (sequence (make-view connection strategy
                              :selector #'channel-timestamps))
         sequence-start-time
         ((&flet sequence-start-time ()
            (or sequence-start-time
                (setf sequence-start-time (unless (emptyp sequence)
                                            (first-elt sequence))))))
         sequence-end-time
         ((&flet sequence-end-time ()
            (or sequence-end-time
                (setf sequence-end-time (unless (emptyp sequence)
                                          (last-elt sequence))))))
         ((&labels index-difference (index timestamp)
            (let ((index-timestamp (elt sequence index)))
              (values (abs (local-time:timestamp-difference
                            timestamp index-timestamp))
                      index-timestamp))))
         ((&labels timestamp->index (timestamp name)
            (log:info "~@<Mapping requested ~A ~A to index (this can ~
                       take a moment)~@:>"
                      name timestamp) ; TODO use progress
            (etypecase timestamp
              (real
               (timestamp->index
                (local-time:adjust-timestamp
                 (if (minusp timestamp)
                     (sequence-end-time)
                     (sequence-start-time))
                 (:offset :sec  (floor timestamp))
                 (:offset :nsec (mod (floor timestamp 1/1000000000)
                                     1000000000)))
                name))
              (local-time:timestamp
               (if-let ((index (when (local-time:timestamp<=
                                      (sequence-start-time)
                                      timestamp
                                      (sequence-end-time))
                                 (position timestamp sequence
                                           :test #'local-time:timestamp<=))))
                 (values
                  (if (and (plusp index)
                           (< (index-difference (1- index) timestamp)
                              (index-difference index      timestamp)))
                      (1- index)
                      index)
                  timestamp)
                 (error "~@<Could not find requested timestamp ~A in ~
                         ~/rsbag.rsb.replay::format-source/ (with ~
                         temporal range [~A, ~A]).~@:>"
                        timestamp connection
                        (sequence-start-time) (sequence-end-time)))))))
         ((&flet check-index (index timestamp name)
            (let+ (((&values difference effective)
                    (index-difference index timestamp)))
              (log:info "~@<Mapped requested ~A ~A to index ~:D (at ~
                         time ~A, ~,6F seconds difference)~@:>"
                        name timestamp index effective difference)
              (when (> difference 1)
                (warn "~@<Mapped ~A ~A is rather far (~F second~:P) from ~
                       requested ~A ~A~@:>"
                      name effective difference name timestamp)))))
         ((&flet warn-missing-times (name time)
            (warn "~@<No start and end times in ~
                   ~/rsbag.rsb.replay::format-source/; ~
                   ignoring requested ~A ~A~@:>"
                  connection name time))))
    (cond
      ((not start-time))
      ((and (sequence-start-time) (sequence-end-time))
       (multiple-value-setq (start-index start-time)
         (timestamp->index start-time "start time"))
       (check-index start-index start-time "start time"))
      (t
       (warn-missing-times start-time "start time")))
    (cond
      ((not end-time))
      ((and (sequence-start-time) (sequence-end-time))
       (multiple-value-setq (end-index end-time)
         (timestamp->index end-time "end time"))
       (check-index end-index end-time "end time"))
      (t
       (warn-missing-times end-time "end time")))))

;;; `repetitions-mixin' mixin class

(defclass repetitions-mixin ()
  ((num-repetitions :initarg  :num-repetitions
                    :type     (or (eql t) positive-integer)
                    :reader   strategy-num-repetitions
                    :initform 1
                    :documentation
                    "Stores the number of times the replay should be
                     repeated or `t' to indicate indefinite
                     repetition."))
  (:documentation
   "This class is intended to be mixed into replay strategy classes
    that allow repeating the event replay multiple times."))

(defun call-repeatedly (spec thunk)
 (etypecase spec
   ((eql t)          (iter (funcall thunk)))
   (positive-integer (iter (repeat spec) (funcall thunk)))))

;;; `view-creation-mixin' mixin class

(defclass view-creation-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
    that have to construct a view sequence for multiple channels. The
    generic function `make-view' can be used to customize this
    behavior. The method for `view-creation-mixin' creates a
    serialized view of events across channels."))

(defmethod make-view ((connection t) (strategy view-creation-mixin)
                      &key
                      (selector (rcurry #'inject-informer connection)))
  ;; Default behavior is serializing events across all channels.
  (let ((channel-connections (connection-connections
                              connection :include-inner? nil)))
    (make-serialized-view (mappend #'connection-channels channel-connections)
                          :selector selector)))

;;; `filtering-mixin' mixin class

(defclass filtering-mixin ()
  ((filter :reader   strategy-filter
           :writer   (setf strategy-%filter)
           :initform nil
           :documentation
           "Stores a filter events have to satisfy in order to be
            replayed."))
  (:documentation
   "Adds the ability to select events for replay using RSB filters."))

(defmethod shared-initialize :after ((instance   filtering-mixin)
                                     (slot-names t)
                                     &key
                                     (filters nil filters-supplied?))
  (when filters-supplied?
    (setf (strategy-%filter instance)
          (rsb.filter:make-filter :and :children filters))))

(defmethod process-event ((connection         replay-bag-connection)
                          (strategy           filtering-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              t)
                          (sink               t))
  (if-let ((filter (strategy-filter strategy)))
    (when (rsb.filter:matches? filter event)
      (call-next-method))
    (call-next-method)))

;;; `sequential-mixin' mixin class

(defclass sequential-mixin (replay-restart-mixin
                            time-bounds-mixin
                            repetitions-mixin
                            view-creation-mixin
                            event-id-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
    that essentially process all events in a sequential manner. The
    method on `replay' for `sequential-mixin' creates a sequence via
    `make-view' and processes all elements of the sequence by
    sequential calls to `process-event'."))

(defmethod replay ((connection t)
                   (strategy   sequential-mixin)
                   &key
                   progress)
  (macrolet ((do-entries (&optional end-index)
               `(iter (when (first-iteration-p)
                        (setf *skip* (lambda () (next-iteration)))
                        (when update-progress
                          (funcall update-progress nil nil)))
                      (for (timestamp event sink) each sequence
                           :from start-index
                           ,@(when end-index '(:below end-index)))
                      (for previous-timestamp previous timestamp)
                      (for i :from start-index)
                      (process-event connection strategy
                                     timestamp previous-timestamp
                                     event sink)
                      (when update-progress
                        (funcall update-progress i timestamp)))))
    (let+ (((&structure-r/o strategy- start-index end-index num-repetitions)
            strategy)
           (sequence        (make-view connection strategy))
           (update-progress (%make-progress-reporter sequence progress))
           ((&flet do-sequence ()
              (if end-index
                  (do-entries end-index)
                  (do-entries)))))
      (call-repeatedly num-repetitions #'do-sequence))))

(defmethod process-event ((connection         t)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              t)
                          (sink               t))
  ;; The default behavior consists in sending EVENT via SINK which is
  ;; assumed to be an `rsb:informer'.
  (let ((no-fill? (ecase (strategy-event-id strategy)
                    (:keep    t)
                    (:replace nil))))
    (rsb:send sink event :unchecked? t :no-fill? no-fill?)))

(defmethod process-event ((connection         t)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              t)
                          (sink               function))
  ;; The default behavior for a function SINK consists in calling SINK
  ;; with EVENT.
  (funcall sink timestamp event))

;;; `timed-replay-mixin' mixin class

(defclass timed-replay-mixin (sequential-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
    which perform time-based scheduling of replayed events."))

(defmethod process-event :before ((connection         t)
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
                    the difference between the scheduled and actual
                    delay. This can become negative when the previous
                    wait executed too slowly.")
   (previous-call  :type     (or null local-time:timestamp)
                   :accessor strategy-previous-call
                   :initform nil
                   :documentation
                   "Stores a timestamp for the previous call to
                    `schedule-event' to estimate how much time
                    actually (as opposed to the scheduled time) passed
                    between the previous and the current call."))
  (:documentation
   "This class is intended to be mixed into replay strategy classes
    that compute an ideal delay between successive events and need to
    have this delay adjusted to compensate for processing
    latencies."))

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
  (let+ (((&structure strategy- previous-delay previous-call) strategy)
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

;;; `delay-limiting-mixin' mixin class

(defclass delay-limiting-mixin ()
  ((max-delay :type     (or null non-negative-real)
              :accessor strategy-max-delay
              :initform nil
              :documentation
              "Maximum delay between adjacent events in seconds."))
  (:documentation
   "This mixin class adds to timed replay strategy classes the ability
    to limit the delays between adjacent events to a particular
    maximum."))

(defmethod shared-initialize :after ((instance   delay-limiting-mixin)
                                     (slot-names t)
                                     &key
                                     (max-delay nil max-deylay-supplied?))
  (when max-deylay-supplied?
    (setf (strategy-max-delay instance) max-delay)))

(defmethod (setf strategy-max-delay) :before ((new-value t)
                                              (strategy  delay-limiting-mixin))
  (check-type new-value (or null non-negative-real)))

(defmethod schedule-event :around ((strategy delay-limiting-mixin)
                                   (event    t)
                                   (previous local-time:timestamp)
                                   (next     local-time:timestamp))
  (if-let ((max-delay (strategy-max-delay strategy)))
    (min (call-next-method) max-delay)
    (call-next-method)))

;;; `speed-adjustment-mixin' mixin class

(defclass speed-adjustment-mixin ()
  ((speed :type     positive-real
          :accessor strategy-speed
          :initform 1
          :documentation
          "Stores the speed factor that should be applied to the
           results of scheduling events."))
  (:documentation
   "This mixin class adds to timed replay strategy classes the ability
    to speed up or slow down replay speed by a constant factor."))

(defmethod shared-initialize :after ((instance   speed-adjustment-mixin)
                                     (slot-names t)
                                     &key
                                     (speed nil max-deylay-supplied?))
  (when max-deylay-supplied?
    (setf (strategy-speed instance) speed)))

(defmethod (setf strategy-speed) :before ((new-value t)
                                          (strategy  speed-adjustment-mixin))
  (check-type new-value positive-real))

(defmethod schedule-event :around ((strategy speed-adjustment-mixin)
                                   (event    t)
                                   (previous local-time:timestamp)
                                   (next     local-time:timestamp))
  (/ (call-next-method) (strategy-speed strategy)))

(defmethod print-items:print-items append ((object speed-adjustment-mixin))
  (let+ (((&structure-r/o strategy- speed) object))
    (unless (= speed 1)
      `((:speed ,speed " ~Fx" ((:after :bounding-indices)))))))

;;; `event-id-mixin' mixin class

(defclass event-id-mixin ()
  ((event-id :initarg  :event-id
             :type     event-id-adjustment
             :accessor strategy-event-id
             :initform :replace
             :documentation
             "Stores the strategy for handling ids of replayed
              events."))
  (:documentation
   "This mixin class adds a strategy for handling ids of replayed
    events."))

(defmethod shared-initialize :after ((instance   event-id-mixin)
                                     (slot-names t)
                                     &key
                                     (event-id nil event-id-supplied?))
  (when event-id-supplied?
    (setf (strategy-event-id instance) event-id)))

(defmethod (setf strategy-event-id) :before ((new-value t)
                                             (strategy  event-id-mixin))
  (check-type new-value event-id-adjustment))

;;; `timestamp-adjustment-mixin' mixin class

(defclass timestamp-adjustment-mixin ()
  ((adjustments :type     list
                :accessor strategy-adjustments
                :initform '()
                :documentation
                "Stores a list of adjustments of the form

                   (TIMESTAMP NEW-VALUE)

                 where TIMESTAMP is a keyword designating a timestamp
                 and NEW-VALUE specifies the new value. Currently,
                 NEW-VALUE can be the symbol :NOW or a
                 `local-time:timestamp' object."))
  (:documentation
   "This mixin class adds the ability to adjust event timestamps
    during replay."))

(defmethod shared-initialize :after ((instance   timestamp-adjustment-mixin)
                                     (slot-names t)
                                     &key
                                     (adjustments '() adjustments-supplied?))
  (when adjustments-supplied?
    (setf (strategy-adjustments instance) adjustments)))

(defmethod (setf strategy-adjustments) :before ((new-value t)
                                                (object    timestamp-adjustment-mixin))
  (check-type new-value list "a list of timestamp adjustment specifications")
  (iter (for spec in new-value)
        (check-type spec timestamp-adjustment-spec)))

(defmethod process-event :before ((connection         t)
                                  (strategy           timestamp-adjustment-mixin)
                                  (timestamp          t)
                                  (previous-timestamp t)
                                  (event              rsb:event)
                                  (sink               t))
  "The default behavior consists in sending EVENT via INFORMER."
  (iter (for (key value) in (strategy-adjustments strategy))
        (setf (rsb:timestamp event key)
              (etypecase value
                (timestamp-adjustment-value/now
                 (local-time:now))

                (timestamp-adjustment-value/copy
                 (let ((key (second value)))
                   (or (rsb:timestamp event key)
                       (error "~@<Event ~A does not have a ~A ~
                               timestamp.~:@>"
                              event key))))

                (timestamp-adjustment-value/delta
                 (let+ (((&values sec nsec) (floor (second value))))
                   (local-time:adjust-timestamp (rsb:timestamp event key)
                     (:offset :sec  sec)
                     (:offset :nsec (floor (* 1000000000 nsec))))))

                (local-time:timestamp
                 value)))))

(defmethod print-items:print-items append ((object timestamp-adjustment-mixin))
  (when-let ((adjustments (strategy-adjustments object)))
    `((:adjustments ,(mapcar #'first adjustments) " ~A"))))

;;; `external-driver-mixin' mixin class

(defclass external-driver-mixin (sequential-mixin)
  ((commands :type     list
             :reader   strategy-commands
             :accessor strategy-%commands
             :initform '()
             :documentation
             "Stores available commands as an alist of items of the
              form

                (NAME . IMPLEMENTING-FUNCTION)."))
  (:documentation
   "This class is intended to be mixed into replay strategy classes
    that depend on some kind of external driver to control iteration
    through the sequential data. Instances store a list of available
    commands which are available for invocation by the external driver
    mechanism. The `replay' method basically retrieves subsequent
    commands and executes them until termination is requested."))

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
                          (rsb:event-data (second (funcall element)))))

    ;; Session
    (:quit           . ,(lambda ()
                          (funcall terminate)
                          (values)))))

(defmethod execute-command ((strategy external-driver-mixin)
                            (command  function))
  (funcall command))

(defmethod replay ((connection t)
                   (strategy   external-driver-mixin)
                   &key
                   progress)
  (let+ (((&structure-r/o strategy- start-index end-index) strategy)
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
         ((&labels step* (back?)
            (setf current (sequence:iterator-step
                           sequence current (xor back? from-end?)))
            (when (end? back?)
              (setf current (sequence:iterator-step
                             sequence current (xor (not back?) from-end?)))
              (error "~@<Attempt to step beyond ~:[end~;beginning~] of ~
                      sequence. Current position ~:D, valid range ~
                      [~:D, ~:D[.~@:>"
                     back? (index) start-index (or end-index (length*))))))
         ((&labels index (&optional relative-to-bounds?)
            (if relative-to-bounds?
                (- (index) start-index)
                (sequence:iterator-index sequence current))))
         ((&flet element ()
            (when (end? nil)
              (error "~@<Attempt to access out-of-bounds ~
                      element. Current position ~:D, valid range ~
                      [~:D, ~:D[~@:>."
                     (index) start-index (or end-index (length*))))
            (sequence:iterator-element sequence current)))
         ((&flet emit ()
            (let+ (((timestamp event sink) (element)))
              (process-event connection strategy
                             timestamp previous-timestamp
                             event sink)
              (setf previous-timestamp timestamp))))
         ((&flet terminate ()
            (setf terminate? t))))

    (setf (strategy-%commands strategy)
          (make-commands strategy sequence
                         :length    #'length*
                         :step      #'step*
                         :index     #'index
                         :element   #'element
                         :emit      #'emit
                         :terminate #'terminate))

    (iter (until terminate?)
          (when (first-iteration-p)
            (setf *skip* (lambda () (next-iteration)))
            (when update-progress
              (funcall update-progress nil nil)))
          (for command next (next-command strategy))
          (execute-command strategy command)
          (when update-progress
            (funcall update-progress
                     (index) (unless (end? nil) (first (element))))))))

;;; Utility functions

(defun check-ordered-indices (earlier later)
  "Signal an error unless EARLIER is smaller than LATER."
  (cond
    ((and (not (minusp earlier)) (not (minusp later)) (not (< earlier later)))
     (error "~@<Invalid relation of indices: index ~:D is not greater ~
             than index ~:D.~@:>"
            later earlier))
    ((and (minusp earlier) (minusp later) (not (< earlier later)))
     (error "~@<Invalid relation of indices: end-relative index ~:D is ~
             not greater than end-relative index ~:D.~@:>"
            later earlier))))

(defun check-ordered-times (earlier later)
  "Signal an error unless EARLIER is earlier than LATER."
  (cond
    ((and (typep earlier 'non-negative-real)
          (typep later 'non-negative-real)
          (not (< earlier later)))
     (error "~@<Invalid relation of times: relative time ~,3F is not ~
             later than relative time ~,3F.~@:>"
            later earlier))
    ((and (typep earlier 'negative-real) (typep later 'negative-real)
          (not (< earlier later)))
     (error "~@<Invalid relation of times: end-relative time ~,3F is ~
             not later than end-relative time ~,3F.~@:>"
            later earlier))
    ((and (typep earlier 'local-time:timestamp)
          (typep later 'local-time:timestamp)
          (not (local-time:timestamp< earlier later)))
     (error "~@<Invalid relation of timestamps: timestamp ~A is not ~
             later than timestamp ~A.~@:>"
            later earlier))))
