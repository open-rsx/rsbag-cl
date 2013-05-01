;;;; bag-connection.lisp --- A class for bag <-> RSB connections.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb)


;;; `bag-connection' class
;;

(defclass bag-connection (rsb.ep:error-policy-mixin)
  ((bag      :initarg  :bag
	     :reader   connection-bag
	     :documentation
	     "Stores the bag object that is connected to RSB
participants as a data source or sink.")
   (channels :initarg  :channels
	     :type     list
	     :reader   connection-channels
	     :initform nil
	     :documentation
	     "Stores a list of channel connections for channels of the
bag that are connected to RSB participants as data sources or
sinks."))
  (:default-initargs
   :bag (missing-required-initarg 'bag-connection :bag))
  (:documentation
   "Instances of this class represent the connections being
established when channels of bags are used as data sources or sinks of
RSB participants. "))

(defmethod shared-initialize :after ((instance   bag-connection)
                                     (slot-names t)
                                     &key)
  (setf (rsb.ep:processor-error-policy instance)
	(rsb.ep:processor-error-policy instance)))

(defmethod (setf rsb.ep:processor-error-policy) :before ((new-value t)
							 (object    bag-connection))
  (iter (for channel in (connection-channels object))
	(setf (rsb.ep:processor-error-policy channel) new-value)))

(defmethod close ((connection bag-connection)
		  &key &allow-other-keys)
  "Close all channel connections, then close the bag."
  (map nil #'close (connection-channels connection))
  (close (connection-bag connection)))

(defmethod wait ((connection bag-connection))
  "Wait for all channel connections."
  (map nil #'wait (connection-channels connection)))

(defmethod start ((connection bag-connection))
  (map nil #'start (connection-channels connection)))

(defmethod stop ((connection bag-connection))
  (map nil #'stop (connection-channels connection)))

(defmethod print-object ((object bag-connection) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (connection-channels object)))))


;;; `replay-bag-connection' class
;;

(defclass replay-bag-connection (bag-connection)
  ((strategy :initarg  :strategy
	     :reader    connection-strategy
	     :documentation
	     "Stores the strategy that is used for replaying events
from the associated bag of the connection."))
  (:default-initargs
   :strategy (missing-required-initarg 'replay-bag-connection :strategy))
  (:documentation
   "Instances of this class associated an event replay strategy, a
source bag and `rsb:informer' instances to collaboratively replay the
events from the bag."))

(defmethod (setf rsb.ep:processor-error-policy) :before ((new-value t)
							 (object    replay-bag-connection))
  (let+ (((&accessors-r/o (strategy connection-strategy)) object))
    (setf (rsb.ep:processor-error-policy strategy) new-value)))
