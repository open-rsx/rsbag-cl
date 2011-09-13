;;; channel-connection.lisp --- A class for bag channel <-> RSB connections.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsbag.rsb)


;;; `channel-connection' class
;;

(defclass channel-connection ()
  ((bag         :initarg  :bag
		:reader connection-bag
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
		  &key &allow-other-keys)
  (detach/ignore-errors (connection-participant connection)))


;;; `recording-channel-connection' class
;;

(defclass recording-channel-connection (channel-connection)
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
                                       &key)
  (bind (((:accessors-r/o (participant connection-participant)) instance))
    (push instance (rsb.ep:handlers participant))))

(defmethod rsb.ep:handle ((sink  recording-channel-connection)
			  (event event))
  (bind (((:accessors-r/o (timestamp connection-timestamp)
			  (strategy  connection-strategy)) sink)
	 ((:values channel found?)
	  (ensure-channel-for sink event strategy)))
    (unless found?
      (push channel (connection-channels sink)))
    (setf (entry channel (timestamp event timestamp)) event)))
