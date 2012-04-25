;;; rsb-event-payload-conversion.lisp --- (De)serialization of RSB events.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsbag.transform)

(defclass rsb-event/payload-conversion (rsb-event)
  ((converter :initarg  :converter
	      :accessor transform-converter
	      :documentation
	      "Stores the converter that should be used
to (de)serialize payloads when (de)serializing events."))
  (:default-initargs
   :converter (required-argument :converter))
  (:documentation
   "Instances of this transform class (de)serialize RSB events from/to
octet vectors like `rsb-event' but additionally (de)serialize
contained payloads using a specified converter."))

(defmethod encode ((transform     rsb-event/payload-conversion)
		   (domain-object rsb:event))
  ;; Encode the payload in-place.
  (bind (((:accessors-r/o (converter transform-converter)) transform))
    (setf (rsb:event-data domain-object)
	  (rsb.converter:domain->wire
	   converter (rsb:event-data domain-object))))
  ;; Forward the modified event to the next method.
  (call-next-method transform domain-object))

(defmethod decode ((transform rsb-event/payload-conversion)
		   (data      simple-array))
  ;; Retrieve event (with encoded payload) from next method and decode
  ;; payload in-place.
  (bind (((:accessors-r/o (wire-schema transform-wire-schema)
			  (converter   transform-converter)) transform)
	 (event (call-next-method)))
    (setf (rsb:event-data event)
	  (rsb.converter:wire->domain
	   converter (rsb:event-data event) wire-schema))
    event))
