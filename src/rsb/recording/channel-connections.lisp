;;;; channel-connections.lisp --- A class for recording channel connections.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.recording)

(defmethod process-event ((connection recording-bag-connection)
                          (timestamp  local-time:timestamp)
                          (event      t))
  (assert (length= 1 (connection-channels connection)))
  (process-event (first (connection-channels connection)) timestamp event))

;;; `recording-channel-connection' class

(defclass recording-channel-connection (channel-connection)
  ((timestamp :initarg  :timestamp
              :type     keyword
              :reader   connection-timestamp
              :initform :create
              :documentation
              "Stores the key of the event timestamp that should be
               used to index events in the associated
               channel. Defaults to the create timestamp.")
   (strategy  :initarg  :strategy
              :reader   connection-strategy
              :documentation
              "Stores a channel allocation/selection strategy."))
  (:default-initargs
   :strategy (missing-required-initarg 'recording-channel-connection :strategy))
  (:documentation
   "Instances of this represent class represent connections being
    established between RSB listeners and bag channels when RSB events
    are recorded into bag channels."))

(defmethod process-event ((connection recording-channel-connection)
                          (timestamp  local-time:timestamp)
                          (event      t))
  (let+ (((&structure-r/o connection- strategy) connection)
         ((&values channel found?)
          (ensure-channel-for connection event strategy)))
    (unless found?
      (push channel (connection-channels connection)))

    (setf (entry channel timestamp) event)))

(defmethod rsb.ep:handle ((sink  recording-channel-connection)
                          (event rsb:event))
  ;; HACK: If necessary, hack our own survey event into compliance.
  (unless (rsb:meta-data event :rsb.transport.wire-schema)
    (setf (rsb:meta-data event :rsb.transport.wire-schema) "void"
          (rsb:event-data event)                           (nibbles:octet-vector)))

  (let+ (((&structure-r/o connection- timestamp) sink))
    (process-event sink (rsb:timestamp event timestamp) event)))

(defmethod print-items:print-items append
    ((object recording-channel-connection))
  `((:timestamp ,(connection-timestamp object) "~A"
                ((:before :channel-count)))))

;;; `recording-participant-channel-connection' class

(defclass recording-participant-channel-connection (participant-channel-connection
                                                    recording-channel-connection)
  ()
  (:documentation
   "Recording channel connection whose endpoint is a participant."))

(defmethod start ((connection recording-participant-channel-connection))
  (let+ (((&accessors-r/o (participant connection-endpoint)) connection))
    (push connection (rsb.ep:handlers participant))))

(defmethod stop ((connection recording-participant-channel-connection))
  (let+ (((&accessors-r/o (participant connection-endpoint)) connection))
    (removef (rsb.ep:handlers participant) connection)))
