;;; channel-connection.lisp --- A class for bag channel <-> RSB connections.
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


;;; `channel-connection' class
;;

(defclass channel-connection ()
  ((bag         :initarg  :bag
		:reader   connection-bag
		:documentation
		"Stores the bag object that is connected to RSB
participants as a data source or sink.")
   (channels    :initarg  :channels
		:type     list
		:accessor connection-channels
		:initform nil
		:documentation
		"Stores the bag channels that are connected to an RSB
participant.")
   (participant :initarg  :participant
		:reader   connection-participant
		:documentation
		"Stores the RSB participant that is connected to a bag
channel."))
  (:default-initargs
   :participant (required-argument :participant))
  (:documentation
   "Instances of this class represent the connections being
established when individual channels of bags are used as data sources
or sinks of RSB participants."))

(defmethod close ((connection channel-connection)
		  &key abort)
  (declare (ignore abort))) ;; nothing to do


;;; `participant-channel-connection' class
;;

(defclass participant-channel-connection (channel-connection)
  ()
  (:documentation
   "A `channel-connection' subclass in which the sink is a RSB
participant."))

(defmethod close ((connection participant-channel-connection)
		  &key abort)
  (declare (ignore abort))
  (detach/ignore-errors (connection-participant connection)))


;;; `recording-channel-connection' class
;;

(defclass recording-channel-connection (participant-channel-connection)
  ((timestamp :initarg  :timestamp
	      :type     keyword
	      :reader   connection-timestamp
	      :initform :create
	      :documentation
	      "Stores the key of the event timestamp that should be
used to index events in the associated channel. Defaults to the create
timestamp.")
   (strategy  :initarg  :strategy
	      :reader   connection-strategy
	      :documentation
	      "Stores a channel allocation/selection strategy."))
  (:default-initargs
   :strategy (required-argument :strategy))
  (:documentation
   "Instances of this represent class represent connections being
established between RSB listeners and bag channels when RSB events are
recorded into bag channels."))

(defmethod initialize-instance :after ((instance recording-channel-connection)
                                       &key
				       (start? t))
  (when start?
    (start instance)))

(defmethod rsb.ep:handle ((sink  recording-channel-connection)
			  (event event))
  (bind (((:accessors-r/o (timestamp connection-timestamp)
			  (strategy  connection-strategy)) sink)
	 ((:values channel found?)
	  (ensure-channel-for sink event strategy)))
    (unless found?
      (push channel (connection-channels sink)))
    (setf (entry channel (timestamp event timestamp)) event)))

(defmethod start ((connection recording-channel-connection))
  (bind (((:accessors-r/o (participant connection-participant)) connection))
    (push connection (rsb.ep:handlers participant))))

(defmethod stop ((connection recording-channel-connection))
  (bind (((:accessors-r/o (participant connection-participant)) connection))
    (removef (rsb.ep:handlers participant) connection)))
