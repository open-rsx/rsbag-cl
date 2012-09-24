;;; channel-connection.lisp --- A class for bag channel <-> RSB connections.
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

(cl:in-package :rsbag.rsb)


;;; `channel-connection' class
;;

(defclass channel-connection (rsb.ep:error-policy-mixin)
  ((bag      :initarg  :bag
	     :reader   connection-bag
	     :documentation
	     "Stores the bag object that is connected to event sources
or sinks.")
   (channels :initarg  :channels
	     :type     list
	     :accessor connection-channels
	     :initform nil
	     :documentation
	     "Stores the bag channels that are connected to event
sources or sinks.")
   (endpoint :initarg  :endpoint
	     :reader   connection-endpoint
	     :documentation
	     "Stores the endpoint that is connected to a bag
channel."))
  (:default-initargs
   :bag      (required-argument :bag)
   :endpoint (required-argument :endpoint))
  (:documentation
   "Instances of this class represent the connections being
established when individual channels of bags are used as data sources
or sinks and connected to event sources or sinks such as RSB
participants."))

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

(defmethod shared-initialize :after ((instance   participant-channel-connection)
                                     (slot-names t)
                                     &key)
  (setf (rsb.ep:processor-error-policy instance)
	(rsb.ep:processor-error-policy instance)))

(defmethod (setf rsb.ep:processor-error-policy) :before ((new-value t)
							 (object    participant-channel-connection))
  (setf (hooks:hook-handlers (rsb:participant-error-hook (connection-endpoint object)))
	(when new-value (list new-value))))

(defmethod close ((connection participant-channel-connection)
		  &key abort)
  (declare (ignore abort))
  (detach/ignore-errors (connection-endpoint connection)))


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
  (let+ (((&accessors-r/o (timestamp connection-timestamp)
			  (strategy  connection-strategy)) sink)
	 ((&values channel found?)
	  (ensure-channel-for sink event strategy)))
    (unless found?
      (push channel (connection-channels sink)))
    (setf (entry channel (timestamp event timestamp)) event)))

(defmethod start ((connection recording-channel-connection))
  (let+ (((&accessors-r/o (participant connection-endpoint)) connection))
    (push connection (rsb.ep:handlers participant))))

(defmethod stop ((connection recording-channel-connection))
  (let+ (((&accessors-r/o (participant connection-endpoint)) connection))
    (removef (rsb.ep:handlers participant) connection)))
