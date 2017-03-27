;;;; protocol.lisp --- Protocol functions used in the rsb module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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
    backends can be inspected using the service designated by
    `rsbag.rsb:channel-strategy'.

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
                         filters
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
    events should be replayed.

    If supplied, FILTERS is a list of RSB filters that events have to
    match in order to be replayed."))

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

;;; Composite connection protocol

(defgeneric connection-direct-connections (connection)
  (:documentation
   "Return the direct child connections of CONNECTION."))

(defgeneric connection-connections (connection
                                    &key
                                    include-inner?
                                    include-self?
                                    leaf-test)
  (:documentation
   "Return a list of certain ancestor connections of CONNECTION.

    INCLUDE-INNER? controls whether inner nodes or only leafs of the
    connection tree are returned.

    INCLUDE-SELF? controls whether CONNECTION is returned.

    LEAF-TEST is a predicate that is called to determine whether a
    given connection should be considered a leaf or an inner node."))

;; Default behavior

(defmethod connection-direct-connections ((connection t))
  '())

(defmethod connection-connections ((connection t)
                                   &key
                                   (include-inner? t)
                                   (include-self?  include-inner?)
                                   (leaf-test      (of-type 'channel-connection)))
  (let ((connections (mappend (rcurry #'connection-connections
                                      :include-inner? include-inner?)
                              (connection-direct-connections connection))))
    (cond
      (include-self?
       (list* connection connections))
      ((funcall leaf-test connection)
       (list connection))
      (t
       connections))))
