;;; protocol.lisp --- Protocol functions used in the rsb module.
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


;;; Connection setup protocol
;;

(defgeneric events->bag (source dest
			 &rest args
			 &key &allow-other-keys)
  (:argument-precedence-order dest source)
  (:documentation
   "Make and return a connection between the RSB participant(s) SOURCE
and the channel or bag DEST. When the connection is established,
events received by SOURCE are stored in DEST. The keyword arguments
ARGS are passed to the function constructing SOURCE, the function
constructing DEST or the new connection depending on their keyword
part."))

(defgeneric bag->events (source dest
			 &rest args
		         &key &allow-other-keys)
  (:documentation
   "Make and return a connection between the channel or bag SOURCE and
the RSB participant(s) DEST. When the connection is established,
events are read from SOURCE and published via DEST. The keyword
arguments ARGS are passed to the function constructing DEST, the
function constructing SOURCE or the new connection depending on their
keyword part."))


;;; Connection protocol
;;

(defgeneric done? (connection)
  (:documentation
   "Return non-nil, if CONNECTION has finished transferring events
from its source to its destination."))

(defgeneric wait (connection)
  (:documentation
   "Wait until CONNECTION finishes transferring events from its source
to its destination, then return."))

;; connections also implement a method on cl:close
