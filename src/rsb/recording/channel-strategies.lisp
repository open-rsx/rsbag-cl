;;;; channel-strategies.lisp --- Strategy classes for allocating channels.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.recording)

;;; `ensure-channel-mixin'

(defclass ensure-channel-mixin ()
  ()
  (:documentation
   "Adds basic {ensure,make}-channel-for behavior to strategy classes."))

(defmethod ensure-channel-for ((connection channel-connection)
                               (event      t)
                               (strategy   ensure-channel-mixin))
  (let+ ((name   (channel-name-for connection event strategy))
         (bag    (connection-bag connection))
         (found? t)
         ((&flet make-channel (condition)
            (declare (ignore condition))
            (setf found? nil)
            (let+ (((&values meta-data transform)
                    (make-channel-for connection event strategy)))
              (invoke-restart 'create meta-data :transform transform)))))
    (values (bag-channel bag name :if-does-not-exist #'make-channel) found?)))

(defmethod make-channel-for ((connection t)
                             (event      t)
                             (strategy   ensure-channel-mixin))
  (let* ((transform (channel-transform-for connection event strategy))
         (meta-data (channel-meta-data-for connection transform event strategy)))
    (values meta-data transform)))

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
    (setf (strategy-%next instance) (make-strategy next))))

(defmethod rsb.ep:access? ((processor delegating-mixin)
                           (part      t)
                           (access    t))
  (rsb.ep:access? (strategy-next processor) part access))

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

(defclass scope-and-type (ensure-channel-mixin)
  ()
  (:documentation
   "Allocates a channel for each combination of scope and wire-schema.

    The channel allocation for a given combination is performed when
    the first event exhibiting that combination is processed. Channel
    names are of the form SCOPE:TYPE where SCOPE is the scope string
    of the received event (including the final \"/\") and TYPE is the
    wire-schema string of the payload of the event.

    As an example, an event on scope /foo/bar/ with wire-schema
    \".rst.vision.Image\" would be stored in a channel called
    \"/foo/bar/:.rst.vision.Image\"."))

(service-provider:register-provider/class
 'strategy :scope-and-type :class 'scope-and-type)

(macrolet ((needs (part)
             `(defmethod rsb.ep:access? ((processor scope-and-type)
                                         (part      (eql ,part))
                                         (access    (eql :read)))
                t)))
  (needs :scope)
  (needs :meta-data))

(defmethod channel-name-for ((connection t)
                             (event      rsb:event)
                             (strategy   scope-and-type))
  (scope+wire-schema->channel-name event))

(defmethod channel-transform-for ((connection t)
                                  (event      rsb:event)
                                  (strategy   scope-and-type))
  (let ((wire-schema (make-keyword (rsb:meta-data
                                    event :rsb.transport.wire-schema))))
    (rsbag.transform:make-transform
     rsbag.transform:+rsb-schema-name+ wire-schema)))

(defmethod channel-meta-data-for ((connection t)
                                  (transform  t)
                                  (event      rsb:event)
                                  (strategy   scope-and-type))
  '())

(defmethod channel-meta-data-for ((connection participant-channel-connection)
                                  (transform  t)
                                  (event      rsb:event)
                                  (strategy   scope-and-type))
  (let+ (((&structure-r/o connection- bag (participant endpoint)) connection)
         ((&accessors-r/o (id rsb:participant-id)) participant)
         (format (channel-format-for bag transform event strategy)))
    (list* :source-name   (princ-to-string id)
           :source-config (princ-to-string
                           (rsb:abstract-uri participant))
           (when format (list :format format)))))

;;; `collapse-reserved'

(defclass collapse-reserved (ensure-channel-mixin
                             delegating-mixin)
  ()
  (:documentation
   "Collapses scopes reserved by RSB into their first three components.

    This, for example, prevents introspection scopes like
    /__rsb/introspection/participants/ID/…  from creating two
    channels (Hello and Bye) for each participant."))

(service-provider:register-provider/class
 'strategy :collapse-reserved :class 'collapse-reserved)

(defmethod rsb.ep:access? ((processor collapse-reserved)
                           (part      (eql :scope))
                           (access    (eql :read)))
  t)

(defmethod channel-name-for ((connection t)
                             (event      rsb:event)
                             (strategy   collapse-reserved))
  (let ((scope (rsb:event-scope event)))
    (if (rsb:sub-scope?/no-coerce scope rsb:*reserved-scope*)
        ;; Collapse reserved scopes to three components.
        (scope+wire-schema->channel-name
         event (rsb:make-scope (subseq (rsb:scope-components scope) 0 3)))
        ;; All other events go into the usual channels.
        (channel-name-for connection event (strategy-next strategy)))))

;;; Utility functions

(defun scope+wire-schema->channel-name (event
                                        &optional
                                        (scope (rsb:event-scope event)))
  (if-let ((wire-schema (rsb:meta-data event :rsb.transport.wire-schema)))
    (format nil "~A:~A" (rsb:scope-string scope) wire-schema)
    (error "~@<Event ~A does not have a ~A meta-data item.~@:>"
           event :rsb.transport.wire-schema)))
