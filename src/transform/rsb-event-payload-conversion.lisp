;;;; rsb-event-payload-conversion.lisp --- (De)serialization of RSB events.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.transform)

(defclass rsb-event/payload-conversion (rsb-event)
  ((converter :initarg  :converter
	      :accessor transform-converter
	      :documentation
	      "Stores the converter that should be used
to (de)serialize payloads when (de)serializing events."))
  (:default-initargs
   :converter (missing-required-initarg
               'rsb-event/payload-conversion :converter))
  (:documentation
   "Instances of this transform class (de)serialize RSB events from/to
octet vectors like `rsb-event' but additionally (de)serialize
contained payloads using a specified converter."))

(defmethod encode ((transform     rsb-event/payload-conversion)
		   (domain-object rsb:event))
  ;; Encode the payload in-place.
  (let+ (((&accessors-r/o (converter transform-converter)) transform))
    (setf (rsb:event-data domain-object)
	  (rsb.converter:domain->wire
	   converter (rsb:event-data domain-object))))
  ;; Forward the modified event to the next method.
  (call-next-method transform domain-object))

(defmethod decode ((transform rsb-event/payload-conversion)
		   (data      simple-array))
  ;; Retrieve event (with encoded payload) from next method and decode
  ;; payload in-place.
  (let+ (((&accessors-r/o (wire-schema transform-wire-schema)
			  (converter   transform-converter)) transform)
	 (event (call-next-method)))
    (setf (rsb:event-data event)
	  (rsb.converter:wire->domain
	   converter (rsb:event-data event) wire-schema))
    event))
