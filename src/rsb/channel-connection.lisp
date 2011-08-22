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

(defclass channel-connection ()
  ((channel     :initarg  :channel
		:reader   connection-channel
		:documentation
		"Stores the bag channel that is connected to an RSB
participant.")
   (participant :initarg  :participant
		:reader   connection-participant
		:documentation
		"Stores the RSB participant that is connected to a bag
channel."))
  (:documentation
   "Instances of this class represent the connections being
established when individual channels of bags are used as data sources
or sinks of RSB informers."))

(defmethod close ((connection channel-connection)
		  &key
		  (close-bag? t)
		  &allow-other-keys)
  (detach (connection-participant connection))
  (when close-bag?
    (close (channel-bag (connection-channel connection)))))
