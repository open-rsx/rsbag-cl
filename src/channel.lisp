;;; channel.lisp --- The channel class represents a time-series of homogeneous data.
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

(defclass channel (#+sbcl standard-object
		   #+sbcl sequence)
  ((name    :initarg  :name
	    :type     string
	    :reader   channel-name
	    :documentation
	    "")
   (id      :initarg  :id
	    :reader   %channel-id
	    :documentation
	    "")
   (backend :initarg  :backend
	    :reader   %channel-backend
	    :documentation
	    ""))
  (:default-initargs
   :name    (required-argument :name)
   :id      (required-argument :id)
   :backend (required-argument :backend))
  (:documentation
   "Instances of this class represent time-series of homogeneous
data items."))

(defmethod channel-timestamps ((channel channel))
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel))
    (get-timestamps backend id)))

(defmethod entry ((channel channel)
		  (index   integer)
		  &key
		  if-does-not-exist)
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel))
    (or (get-entry backend id index)
	(ecase if-does-not-exist
	  (:error (error "No entry for index ~D" index))
	  ((nil)  nil)))))

(defmethod entry ((channel   channel)
		  (timestamp local-time:timestamp)
		  &key
		  if-does-not-exist)
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel))
    (or (get-entry backend id timestamp)
	(case if-does-not-exist
	  (:error (error "No entry for timestamp ~A" timestamp))
	  ((nil)  nil)))))

(defmethod (setf entry) ((new-value t)
			 (channel   channel)
			 (index     local-time:timestamp)
			 &key
			 if-exists)
  (when (eq if-exists :supersede)
    (error "Supersede entries is not supported yes"))

  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel))
    (put-entry backend id index new-value)
    new-value))

(defmethod print-object ((object channel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S (~D)"
	    (channel-name object)
	    (length       object))))


;;; Sequence protocol
;;

#+sbcl
(defmethod sequence:length ((channel channel))
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel))
    (get-num-entries backend id)))

#+sbcl
(defmethod sequence:elt ((channel channel)
			 (index   integer))
  (entry channel index))