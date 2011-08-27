;;; bag-connection.lisp --- A class for bag <-> RSB connections.
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


;;; `bag-connection' class
;;

(defclass bag-connection ()
  ((bag      :initarg :bag
	     :reader  connection-bag
	     :documentation
	     "Stores the bag object that is connected to RSB
participants as a data source or sink.")
   (channels :initarg :channels
	     :type    list
	     :reader  connection-channels
	     :documentation
	     "Stores a list of channel connections for channels of the
bag that are connected to RSB participants as data sources or
sinks."))
  (:documentation
   "Instances of this class represent the connections being
established when channels of bags are used as data sources or sinks of
RSB participants. "))

(defmethod close ((connection bag-connection)
		  &key &allow-other-keys)
  "Close all channel connections, then close the bag."
  (bind (((:accessors-r/o (bag      connection-bag)
			  (channels connection-channels)) connection))
    (map nil (rcurry #'close :close-bag? nil) channels)
    (close bag)))

(defmethod wait ((connection bag-connection))
  "Wait for all channel connections."
  (map nil #'wait (connection-channels connection)))

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
   :strategy (required-argument :strategy))
  (:documentation
   "Instances of this class associated an event replay strategy, a
source bag and `rsb:informer' instances to collaboratively replay the
events from the bag."))
