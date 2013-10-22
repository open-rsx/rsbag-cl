;;;; channel-strategies.lisp --- Strategy classes for allocating channels.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; `scope-and-type' channel allocation strategy class

(defmethod find-channel-strategy-class ((spec (eql :scope-and-type)))
  (find-class 'scope-and-type))

(defclass scope-and-type ()
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
  (if-let ((scope       (scope-string (event-scope event)))
           (wire-schema (rsb:meta-data event :rsb.transport.wire-schema)))
    (format nil "~A:~A" scope wire-schema)
    (error "~@<Event ~A does not have a ~A meta-data item.~@:>"
           event :rsb.transport.wire-schema)))

;; TODO(jmoringe, 2012-02-17): move to protocol or mixin
(defmethod ensure-channel-for ((connection channel-connection)
                               (event      event)
                               (strategy   scope-and-type))
  (let* ((name    (channel-name-for connection event strategy))
         (bag     (connection-bag connection))
         (channel (bag-channel bag name :if-does-not-exist nil)))
    (if channel
        (values channel t)
        (make-channel-for connection event strategy))))

(defmethod make-channel-for ((connection participant-channel-connection)
                             (event      event)
                             (strategy   scope-and-type))
  (let+ (((&accessors-r/o (bag         connection-bag)
                          (participant connection-endpoint)) connection)
         ((&accessors-r/o (id participant-id)) participant)
         (name        (channel-name-for connection event strategy))
         (wire-schema (make-keyword (rsb:meta-data event :rsb.transport.wire-schema)))
         (transform   (make-transform +rsb-schema-name+ wire-schema))
         (format      (channel-format-for bag transform event strategy)))
    (setf (bag-channel bag name :transform transform)
          (append
           (list :source-name   (princ-to-string id)
                 :source-config (princ-to-string
                                 (abstract-uri participant)))
           (when format
             (list :format format))))))
