;;; protocol.lisp --- Protocol used by the cl-rsbag system.
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

(in-package :rsbag)


;;; Back opening protocol
;;

(defgeneric open-bag (source
		      &rest args
		      &key
		      backend
		      &allow-other-keys)
  (:documentation
   "Open the data source SOURCE and return a bag object using the
backend designated by BACKEND and passing ARGS (expect the keyword
argument :backend) to the backend.

Example:
RSBAG> (open-bag #p\"/tmp/mylog.tide\" :backend :tidelog)
#<BAG (1) {}>"))


;;; Default behavior
;;

(defmethod open-bag ((source pathname)
		     &rest args
		     &key
		     (backend (error (required-argument :backend))))
  (let* ((stream  (open source
			     :element-type      '(unsigned-byte 8)
			     :direction         :io
			     :if-exists         :append
			     :if-does-not-exist :create))
	 (backend (apply #'make-instance
			 backend
			 :stream stream
			 (remove-from-plist args :backend :stream))))
    (make-instance 'bag :backend backend)))

(defmethod open-bag ((source string)
		     &rest args
		     &key &allow-other-keys)
  (apply #'open-bag (parse-namestring source) args))


;;; Bag protocol
;;

(defgeneric bag-channels (bag)
  (:documentation
   "Return a list of the `channel's stored in bag."))

(defgeneric bag-channel (bag name
			 &key
			 if-does-not-exist)
  (:documentation
   "Return the `channel' named NAME in BAG.
The value of IF-DOES-NOT-EXIST controls the behavior in case the
requested channel does not exist. Valid values are :error, which
causes an error to be signaled and nil, which causes nil to be
returned."))

(defgeneric (setf bag-channel) (spec bag name
				&key
				if-exists)
  (:documentation
   "Add or update and return the channel named NAME in BAG. SPEC is a
plist which specifies properties of the created or updated
channel. IF-EXISTS controls the behavior in case a channel named NAME
already exists in BAG. Valid values are :error, which causes an error
to be signaled, and :supersede, which causes the existing channel to
be updated."))


;;; Bag behind-the-scenes protocol ;)
;;

(defgeneric %channel-class (bag)
  (:documentation
   "Return the channel class used by bag."))

(defgeneric %make-channel (bag name meta-data
			   &optional
			   id)
  (:documentation
   "Create and return a new channel named NAME with id ID and
associated meta-data META-DATA for BAG. The returned object has to
implement the channel protocol."))


;;; Channel protocol
;;

(defgeneric channel-name (channel)
  (:documentation
   "Return the name of CHANNEL."))

(defgeneric channel-timestamps (channel)
  (:documentation
   "Return a sequence of `local-time:timestamp' objects representing
the points in time for which CHANNEL contains entries."))

(defgeneric entry (channel index
		   &key
		   if-does-not-exist)
  (:documentation
   "Return the entry at position or time INDEX in CHANNEL. If INDEX is
an integer, the INDEX-th entry is returned. If INDEX is a
`local-time:timestamp' instances, the entry stored for the point in
time represented by INDEX is returned. IF-DOES-NOT-EXIST controls the
behavior in case there is no entry for INDEX. Valid values are nil,
which causes nil to be returned and :error, which causes an error to
be signaled."))

(defgeneric (setf entry) (new-value channel index
			  &key
			  if-exists)
  (:documentation
   "Store NEW-VALUE as the value of the entry at position or time
INDEX in CHANNEL. If INDEX is an integer, the INDEX-th entry is
returned. If INDEX is a `local-time:timestamp' instances, the entry
stored for the point in time represented by INDEX is
returned. IF-EXISTS controls the behavior in case an entry is already
stored at INDEX. Valid values are :error, which causes an error to be
signaled, and :supersede, which causes the stored value to be
replaced."))

;; When available, channels additionally implement the sequence
;; protocol such that the channel appears as a sequence of its
;; entries.