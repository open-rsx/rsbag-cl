;;; channel-strategies.lisp --- Strategy classes for allocating channels.
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

(cl:in-package :rsbag.rsb)


;;; `scope-and-type' channel allocation strategy class
;;

(defmethod find-channel-strategy-class ((spec (eql :scope-and-type)))
  (find-class 'scope-and-type))

(defclass scope-and-type ()
  ()
  (:documentation
   "This strategy allocates a separate channel for each combination of
RSB scope and wire-schema. The channel allocation for a given
combination is performed when the first event exhibiting that
combination is processed.

As an example, an event on scope /foo/bar/ with wire-schema
\".rst.Image\" would be stored in a channel called
\"/foo/bar/:.rst.Image\"."))

(defmethod channel-name-for ((connection channel-connection)
			     (event      event)
			     (strategy   scope-and-type))
  (if-let ((scope       (scope-string (event-scope event)))
	   (wire-schema (rsb:meta-data event :rsb.transport.wire-schema)))
    (format nil "~A:~A" scope wire-schema)
    (error "~@<Event ~A does not have a ~A meta-data item.~@:>"
	   event :rsb.transport.wire-schema)))

(defmethod ensure-channel-for ((connection channel-connection)
			       (event      event)
			       (strategy   scope-and-type))
  (let* ((name    (channel-name-for connection event strategy))
	 (bag     (connection-bag connection))
	 (channel (bag-channel bag name :if-does-not-exist nil)))
    (if channel
	(values channel t)
	(make-channel-for connection event strategy))))

(defmethod make-channel-for ((connection channel-connection)
			     (event      event)
			     (strategy   scope-and-type))
  (bind (((:accessors-r/o (bag         connection-bag)
			  (participant connection-participant)) connection)
	 ((:accessors-r/o (id participant-id)) participant)
	 (name        (channel-name-for connection event strategy))
	 (format      (channel-format-for connection event strategy))
	 (wire-schema (make-keyword (rsb:meta-data event :rsb.transport.wire-schema)))
	 (transform   (make-transform :rsb-event wire-schema)))
    (setf (bag-channel bag name :transform transform)
	  (append
	   (list :source-name   (princ-to-string id)
		 :source-config (princ-to-string
				 (abstract-uri participant)))
	   (when format
	     (list :format format))))))
