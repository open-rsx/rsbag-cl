;;; protocol.lisp --- Backend protocol of the cl-rsbag system.
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

(cl:in-package :rsbag.backend)


;;; Backend protocol
;;

(defgeneric source-name (backend)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric get-channels (backend)
  (:documentation
   "Return the list of channels that are stored in the data source
represented by BACKEND. Elements of the list are of the form (ID NAME
META-DATA)."))

(defgeneric make-channel-id (backend name)
  (:documentation
   "Return an object suitable for uniquely identifying a channel
within BACKEND. The object may be based on NAME."))

(defgeneric put-channel (backend channel name meta-data)
  (:documentation
   "Add a channel name NAME identified by CHANNEL with meta-data
META-DATA to the list of channels that are stored in the data source
represented by BACKEND."))

(defgeneric get-num-entries (backend channel)
  (:documentation
   "Return the number of entries stored in CHANNEL of the data source
represented by BACKEND."))

(defgeneric get-timestamps (backend channel)
  (:documentation
   "Return a list of the timestamps for which entries are stored in
CHANNEL of the data source represented by BACKEND."))

(defgeneric get-entry (backend channel index)
  (:documentation
   "Retrieve and return the entry designated by INDEX of CHANNEL in
the data source represented by BACKEND."))

(defgeneric put-entry (backend channel index entry)
  (:documentation
   "Store ENTRY at the position designated by INDEX in CHANNEL in the
data source represented by BACKEND."))


;;; Stream protocol
;;

(defgeneric backend-stream (backend)
  (:documentation
   "Return the stream of the data source of BACKEND."))


;;; Buffering protocol
;;

(defgeneric make-buffer (backend previous)
  (:documentation
   "Allocate and return a suitable buffer for BACKEND based on the
buffer PREVIOUS. PREVIOUS can be nil, for the initial allocation."))

(defgeneric write-buffer (backend buffer)
  (:documentation
   "Commit the entries accumulated in BUFFER to the data source
represented by BACKEND."))


;;; Finding backend classes
;;

(dynamic-classes:define-findable-class-family backend
    "This class family consists of file format backends.")
