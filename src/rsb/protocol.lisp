;;;; protocol.lisp --- Protocol functions used in the rsb module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; Connection setup protocol

(defgeneric events->bag (source dest
                         &rest args
                         &key
                         error-policy
                         transports
                         filters
                         timestamp
                         if-exists
                         backend
                         bag-class
                         channel-strategy
                         start?
                         &allow-other-keys)
  (:argument-precedence-order dest source)
  (:documentation
   "Make and return a connection between the RSB participant(s) SOURCE
    and the channel or bag DEST. When the connection is established,
    events received by SOURCE are stored in DEST. The keyword
    arguments ARGS are passed to the function constructing SOURCE, the
    function constructing DEST or the new connection depending on
    their keyword part.

    If supplied, ERROR-POLICY has to be nil or a function to be called
    with a `condition' object when an error is signaled.

    If supplied, TRANSPORTS configures RSB transport mechanisms. See
    `rsb:make-participant' for details.

    FILTERS is a list of RSB filters (which can be ordinary functions)
    that are applied to discriminate events for recording. Only events
    matching all elements of FILTERS are recorded.

    If supplied, TIMESTAMP has to be a keyword and selects the RSB
    event timestamp that should be used for indexing the recorded
    events. Canonical RSB timestamp names are :create, :send, :receive
    and :deliver, but other user provided timestamp can be used as
    well.

    IF-EXISTS specifies the behavior DEST already exists. Valid values
    are :error and :overwrite. See `rsbag:open-bag' for more
    information.

    BACKEND can be used to explicitly select a file-format backend for
    DEST. If supplied, it has to be a keyword designating a
    file-format backend. Available backends can be inspected using the
    service designated by `rsbag.backend:backend'.

    If supplied, BAG-CLASS selects the class of the bag created for
    DEST. Note that a bag class capable of handling concurrent
    accesses has to be selected if SOURCE amounts to multiple sources.

    If supplied, CHANNEL-STRATEGY selects a channel allocation
    strategy which is responsible for adding channels to DEST when
    events cannot be stored in any of the existing channels. Available
    strategies can be inspected using
    `rsbag.rsb:channel-strategy-classes'.

    If supplied, START? controls whether the recording should start
    immediately. The default behavior is to start immediately."))

(defgeneric bag->events (source dest
                         &rest args
                         &key
                         error-policy
                         backend
                         bag-class
                         replay-strategy
                         start-time
                         start-index
                         end-time
                         end-index
                         channels
                         &allow-other-keys)
  (:documentation
   "Make and return a connection between the channel or bag SOURCE and
    the RSB participant(s) DEST. When the connection is established,
    events are read from SOURCE and published via DEST. The keyword
    arguments ARGS are passed to the function constructing DEST, the
    function constructing SOURCE or the new connection depending on
    their keyword part.

    If supplied, ERROR-POLICY has to be nil or a function to be called
    with a `condition' object when an error is signaled.

    BACKEND can be used to explicitly select a file-format backend for
    SOURCE. If supplied, it has to be a keyword designating a
    file-format backend. Available backends can be inspected using the
    service designated by `rsbag.backend:backend'.

    If supplied, BAG-CLASS selects the class of the bag created for
    SOURCE. See `rsbag:open-bag'.

    If supplied, REPLAY-STRATEGY selects a replay strategy that
    controls the replay timing and coordinates the publishing of
    events via informers.

    START-INDEX and END-INDEX can be used to select a range of stored
    events for replay. The default behavior consists in replaying all
    stored events.

    Similarly, START-TIME and END-TIME can be used to select a range
    of stored events for replay based on temporal bounds. Note that
    the actual start and end of the replay correspond to the events in
    SOURCE which are closest to START-TIME and END-TIME
    respectively. Times can be specified as `local-time:timestamp'
    instances and real numbers. The latter are treated as offsets from
    the start of the recorded date when non-negative and from the end
    of the recorded data when negative.

    If supplied, CHANNELS selects a subset of channels from which
    events should be replayed."))

;;; Connection protocol

(defgeneric connection-bag (connection)
  (:documentation
   "Return the associated bag of CONNECTION."))

(defgeneric done? (connection)
  (:documentation
   "Return non-nil, if CONNECTION has finished transferring events
    from its source to its destination."))

(defgeneric wait (connection)
  (:documentation
   "Wait until CONNECTION finishes transferring events from its source
    to its destination, then return."))

(defgeneric start (connection)
  (:documentation
   "Start recording events received by the associated participant of
    CONNECTION into the associated bag of CONNECTION. Recording can be
    stopped temporarily using the `stop' function or permanently using
    `close'."))

(defgeneric stop (connection)
  (:documentation
   "Stop recording events received by the associated participant of
    CONNECTION into the associated bag of CONNECTION. Recording can be
    continue using the `start' function."))

;; connections also implement a method on cl:close

;;; Replay protocol

(defgeneric replay (connection strategy
                    &key
                    progress)
  (:documentation
   "Replay the events contained in the associated bag of CONNECTION
    according to STRATEGY. Usually, STRATEGY will mostly influence the
    timing of the replay. However, things like simulated loss of
    events or transformations are also possible.

    If PROGRESS is non-nil it has to be a function accepting five
    arguments: progress ratio, current index, start index, end index
    and current timestamp.

    May signal a `replay-error'. In particular,
    `entry-retrieval-error' and `entry-processing-error' may be
    signaled."))

(define-condition-translating-method replay ((connection t) (strategy t)
                                             &key &allow-other-keys)
  (((and error (not replay-error)) entry-retrieval-error)
   :connection connection
   :strategy   strategy))

;;; View creation protocol

(defgeneric make-view (connection strategy)
  (:documentation
   "Make and return a sequence view of the events associated to
    CONNECTION for replay according to STRATEGY."))

;;; Sequential processing protocol

(defgeneric process-event (connection strategy
                           timestamp previous-timestamp
                           event sink)
  (:documentation
   "Process the tuple (TIMESTAMP PREVIOUS-TIMESTAMP EVENT SINK),
    originating from CONNECTION, according to STRATEGY."))

(define-condition-translating-method process-event ((connection         t)
                                                    (strategy           t)
                                                    (timestamp          t)
                                                    (previous-timestamp t)
                                                    (event              t)
                                                    (sink               t))
  ((error entry-processing-error)
   :connection connection
   :strategy   strategy
   :entry      event))

;;; Timed replay protocol

(defgeneric schedule-event (strategy event previous next)
  (:documentation
   "Return a relative time in seconds at which EVENT should be
    replayed according to STRATEGY given timestamps PREVIOUS and NEXT
    of the previous and current event when recorded. PREVIOUS can be
    nil at the start of a replay."))

(defmethod schedule-event ((strategy t)
                           (event    t)
                           (previous (eql nil))
                           (next     local-time:timestamp))
  0)

;;; Replay strategy class family

(service-provider:define-service replay-strategy
  (:documentation
   "Providers implement event replay strategies.

    The main difference between strategies is the handling of
    timing."))

(defgeneric make-replay-strategy (spec &rest args)
  (:documentation
   "Return (potentially creating it first) an instance of the replay
    strategy designated by SPEC."))

(defmethod make-replay-strategy ((spec standard-object) &rest args)
  (if args
      (apply #'reinitialize-instance spec args)
      spec))

(defmethod make-replay-strategy ((spec symbol) &rest args)
  (if (keywordp spec)
      (apply #'service-provider:make-provider 'replay-strategy spec
             args)
      (let ((provider (find spec (service-provider:service-providers 'replay-strategy)
                            :key  (compose #'class-name
                                           #'service-provider:provider-class)
                            :test #'eq)))
        (apply #'service-provider:make-provider 'replay-strategy provider
               args))))

(defmethod make-replay-strategy ((spec class) &rest args)
  (let ((provider (find spec (service-provider:service-providers 'replay-strategy)
                        :key  #'service-provider:provider-class
                        :test #'eq)))
    (apply #'service-provider:make-provider 'replay-strategy provider
           args)))

(defmethod make-replay-strategy ((spec cons) &rest args)
  (apply #'make-replay-strategy (first spec) (append (rest spec) args)))

;;; Channel allocation protocol

(defgeneric channel-name-for (connection event strategy)
  (:documentation
   "Return a channel name string designating the channel within
    CONNECTION in which EVENT should be stored according to
    STRATEGY."))

(defgeneric channel-transform-for (connection event strategy)
  (:documentation
   "Derive, construct and return a transform for the channel within
    CONNECTION in which EVENT should be stored according to
    STRATEGY."))

(defgeneric channel-format-for (connection transform event strategy)
  (:documentation
   "Return a representation of the type of data/serialization
    mechanism according to which the data of EVENT, after being
    encoded by TRANSFORM, will be stored in the channel within
    CONNECTION allocated by STRATEGY."))

(defgeneric channel-meta-data-for (connection transform event strategy)
  (:documentation
   "Construct and return a meta-data plist for the channel within
    CONNECTION in which EVENT should be stored according to STRATEGY
    taking into account TRANSFORM, the associated transform for the
    channel."))

(defgeneric make-channel-for (connection event strategy)
  (:documentation
   "Make and return a channel in CONNECTION in which EVENT can be
    stored according to STRATEGY."))

(defgeneric ensure-channel-for (connection event strategy)
  (:documentation
   "Find or make and return a channel in CONNECTION in which EVENT can
    be stored according to STRATEGY."))

;;; Default behavior

(defmethod channel-format-for ((connection t)
                               (transform  (eql nil))
                               (event      t)
                               (strategy   t))
  ;; Default behavior is to not associate a channel format.
  nil)

(defmethod channel-format-for ((connection t)
                               (transform  t)
                               (event      t)
                               (strategy   t))
  ;; Default behavior for non-nil TRANSFORM is to retrieve the channel
  ;; format from TRANSFORM.
  (transform-format transform))

;;; Channel allocation strategy class family

(dynamic-classes:define-findable-class-family channel-strategy
    "This family consists of classes that implement channel selection
     and allocation strategies.")

(defgeneric make-channel-strategy (thing &rest args)
  (:documentation
   "Return (potentially creating it first) an instance of the channel
    strategy designated by THING."))

(defmethod make-channel-strategy ((thing symbol) &rest args)
  (apply #'make-channel-strategy
         (if (keywordp thing)
             (find-channel-strategy-class thing)
             (find-class thing))
         args))

(defmethod make-channel-strategy ((thing class) &rest args)
  (apply #'make-instance thing args))

(defmethod make-channel-strategy ((thing t) &rest args)
  (declare (ignore args))
  thing)
