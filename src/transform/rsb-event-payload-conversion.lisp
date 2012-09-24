;;; rsb-event-payload-conversion.lisp --- (De)serialization of RSB events.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
