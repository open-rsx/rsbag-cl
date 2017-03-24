;;;; channel-connection.lisp --- A class for bag channel <-> RSB connections.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; `channel-connection' class

(defclass channel-connection (rsb.ep:error-policy-mixin
                              print-items:print-items-mixin)
  ((bag      :initarg  :bag
             :reader   connection-bag
             :documentation
             "Stores the bag object that is connected to event sources
              or sinks.")
   (channels :initarg  :channels
             :type     list
             :accessor connection-channels
             :initform '()
             :documentation
             "Stores the bag channels that are connected to event
              sources or sinks by the connection."))
  (:default-initargs
   :bag (missing-required-initarg 'channel-connection :bag))
  (:documentation
   "Connection between bag channels and an event source or sink."))

(defmethod close ((connection channel-connection) &key abort)
  (declare (ignore abort))) ; nothing to do

(defmethod print-items:print-items append ((object channel-connection))
  `((:channel-count ,(length (connection-channels object)) " (~D)")))

;;; `endpoint-channel-connection' class

(defclass endpoint-channel-connection (channel-connection)
  ((endpoint :initarg  :endpoint
             :reader   connection-endpoint
             :documentation
             "Stores the endpoint that is connected to a bag
              channel."))
  (:default-initargs
   :endpoint (missing-required-initarg 'endpoint-channel-connection :endpoint))
  (:documentation
   "Instances of this class represent the connections being
    established when individual channels of bags are used as data
    sources or sinks and connected to event sources or sinks such as
    functions or RSB participants."))

;;; `participant-channel-connection' class

(defclass participant-channel-connection (endpoint-channel-connection)
  ()
  (:documentation
   "A `channel-connection' with an RSB participant endpoint."))

(defmethod shared-initialize :after ((instance   participant-channel-connection)
                                     (slot-names t)
                                     &key)
  (setf (rsb.ep:processor-error-policy instance)
        (rsb.ep:processor-error-policy instance)))

(defmethod (setf rsb.ep:processor-error-policy) :before
    ((new-value t)
     (object    participant-channel-connection))
  (setf (hooks:hook-handlers (rsb:participant-error-hook (connection-endpoint object)))
        (when new-value (list new-value))))

(defmethod close ((connection participant-channel-connection) &key abort)
  (declare (ignore abort))
  (detach/ignore-errors (connection-endpoint connection)))
