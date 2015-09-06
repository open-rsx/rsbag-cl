;;;; channel-strategies.lisp --- Strategy classes for allocating channels.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; `ensure-channel-mixin'

(defclass ensure-channel-mixin ()
  ()
  (:documentation
   "Adds basic {ensure,make}-channel-for behavior to strategy classes."))

(defvar *name* nil)

(defmethod ensure-channel-for ((connection channel-connection)
                               (event      event)
                               (strategy   ensure-channel-mixin))
  (let* ((name    (channel-name-for connection event strategy))
         (bag     (connection-bag connection))
         (channel (bag-channel bag name :if-does-not-exist nil)))
    (if channel
        (values channel t)
        (let ((*name* name))
          (make-channel-for connection event strategy)))))

(defmethod make-channel-for ((connection participant-channel-connection)
                             (event      event)
                             (strategy   ensure-channel-mixin))
  (let+ (((&structure-r/o connection- bag) connection)
         (name (or *name* (channel-name-for connection event strategy)))
         (transform (channel-transform-for connection event strategy))
         (meta-data (channel-meta-data-for connection transform event strategy)))
    (setf (bag-channel bag name :transform transform) meta-data)))

;;; `delegating-mixin'

(defclass delegating-mixin ()
  ((next :reader   strategy-next
         :accessor strategy-%next
         :documentation
         "Stores a strategy that should be used to perform operations
          not or partially implemented by this strategy."))
  (:default-initargs
   :next (missing-required-initarg 'delegating-mixin :next))
  (:documentation
   "Adds delegation of operations to another strategy instance.

    Intended to be mixed into strategy classes that implement only
    some aspects of the protocol and delegate other aspect to a
    different strategy."))

(defmethod shared-initialize :after ((instance    delegating-mixin)
                                     (slots-names t)
                                     &key
                                     (next nil next-supplied?))
  (when next-supplied?
    (setf (strategy-%next instance) (make-channel-strategy next))))

(defmethod channel-name-for ((connection t)
                             (event      t)
                             (strategy   delegating-mixin))
  (channel-name-for connection event (strategy-next strategy)))

(defmethod channel-transform-for ((connection t)
                                  (event      t)
                                  (strategy   delegating-mixin))
  (channel-transform-for connection event (strategy-next strategy)))

(defmethod channel-format-for ((connection t)
                               (transform  t)
                               (event      t)
                               (strategy  delegating-mixin))
  (channel-format-for connection transform event (strategy-next strategy)))

(defmethod channel-meta-data-for ((connection t)
                                  (transform  t)
                                  (event      t)
                                  (strategy   delegating-mixin))
  (channel-meta-data-for connection transform event (strategy-next strategy)))

;;; `scope-and-type' channel allocation strategy class

(defmethod find-channel-strategy-class ((spec (eql :scope-and-type)))
  (find-class 'scope-and-type))

(defclass scope-and-type (ensure-channel-mixin)
  ()
  (:documentation
   "This strategy allocates a separate channel for each combination of
    RSB scope and wire-schema. The channel allocation for a given
    combination is performed when the first event exhibiting that
    combination is processed. Channel names are of the form SCOPE:TYPE
    where SCOPE is the scope string of the received event (including
    the final \"/\") and TYPE is the wire-schema string of the payload
    of the event.

    As an example, an event on scope /foo/bar/ with wire-schema
    \".rst.vision.Image\" would be stored in a channel called
    \"/foo/bar/:.rst.vision.Image\"."))

(defmethod channel-name-for ((connection channel-connection)
                             (event      event)
                             (strategy   scope-and-type))
  (scope+wire-schema->channel-name event))

(defmethod channel-transform-for ((connection participant-channel-connection)
                                  (event      event)
                                  (strategy   scope-and-type))
  (let ((wire-schema (make-keyword (rsb:meta-data
                                    event :rsb.transport.wire-schema))))
    (make-transform +rsb-schema-name+ wire-schema)))

(defmethod channel-meta-data-for ((connection participant-channel-connection)
                                  (transform  t)
                                  (event      event)
                                  (strategy   scope-and-type))
  (let+ (((&structure-r/o connection- bag (participant endpoint)) connection)
         ((&structure-r/o participant- id) participant)
         (format (channel-format-for bag transform event strategy)))
    (append (list :source-name   (princ-to-string id)
                  :source-config (princ-to-string
                                  (abstract-uri participant)))
            (when format (list :format format)))))

;;; `collapse-reserved'

(defmethod find-channel-strategy-class ((spec (eql :collapse-reserved)))
  (find-class 'collapse-reserved))

(defclass collapse-reserved (ensure-channel-mixin
                             delegating-mixin)
  ()
  (:default-initargs
   :next (missing-required-initarg 'collapse-reserved :next))
  (:documentation
   "Collapsed scopes reserved by RSB into their first three components.

    This, for example, prevents introspection scopes like
    /__rsb/introspection/participants/ID/…  from creating two
    channels (Hello and Bye) for each participant."))

(defmethod channel-name-for ((connection channel-connection)
                             (event      event)
                             (strategy   collapse-reserved))
  (let ((scope (event-scope event)))
    (if (sub-scope?/no-coerce scope *reserved-scope*)
        ;; Collapse reserved scopes to three components.
        (scope+wire-schema->channel-name
         event (make-scope (subseq (scope-components scope) 0 3)))
        ;; All other events go into the usual channels.
        (channel-name-for connection event (strategy-next strategy)))))

;;; Utility functions

(defun scope+wire-schema->channel-name (event
                                        &optional
                                        (scope (event-scope event)))
  (if-let ((wire-schema (rsb:meta-data event :rsb.transport.wire-schema)))
    (format nil "~A:~A" (scope-string scope) wire-schema)
    (error "~@<Event ~A does not have a ~A meta-data item.~@:>"
           event :rsb.transport.wire-schema)))
