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

(defclass channel (plist-meta-data-mixin
		   #+sbcl sequence)
  ((bag       :initarg  :bag
	      :reader   channel-bag
	      :documentation
	      "Stores the bag instance in which this channel is
contained.")
   (name      :initarg  :name
	      :type     string
	      :reader   channel-name
	      :documentation
	      "")
   (meta-data :reader   channel-meta-data)
   (transform :initarg  :transform
	      :reader   channel-transform
	      :initform nil
	      :documentation
	      "")
   (id        :initarg  :id
	      :reader   %channel-id
	      :documentation
	      "")
   (backend   :initarg  :backend
	      :reader   %channel-backend
	      :documentation
	      ""))
  (:default-initargs
   :bag     (required-argument :bag)
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
		  if-does-not-exist
		  (transform        (channel-transform channel)))
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel)
	 (raw (or (get-entry backend id index)
		  (ecase if-does-not-exist
		    (:error (error 'no-such-entry
				   :bag     (channel-bag channel)
				   :channel channel
				   :key     index))
		    ((nil)  nil)))))
    (if transform
	(decode transform raw)
	raw)))

(defmethod entry ((channel   channel)
		  (timestamp local-time:timestamp)
		  &key
		  if-does-not-exist)
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel))
    (or (get-entry backend id timestamp)
	(case if-does-not-exist
	  (:error (error 'no-such-entry
			 :bag     (channel-bag channel)
			 :channel channel
			 :key     timestamp))
	  ((nil)  nil)))))

(defmethod (setf entry) :before ((new-value t)
				 (channel   t)
				 (index     t)
				 &key &allow-other-keys)
  (when (eq (bag-direction (channel-bag channel)) :input)
    (error 'read-only-bag
	   :bag (channel-bag channel))))

(defmethod (setf entry) ((new-value t)
			 (channel   channel)
			 (index     local-time:timestamp)
			 &key
			 if-exists
			 (transform (channel-transform channel)))
  (when (eq if-exists :supersede)
    (error "Supersede entries is not supported yes"))

  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel)
	 (raw (if transform
		  (rsbag.transform:encode transform new-value)
		  new-value)))
    (put-entry backend id index raw)
    new-value))

(defmethod print-object ((object channel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S (~D)"
	    (channel-name object)
	    (length       object))))


;;; Time range protocol
;;

(defmethod start ((channel channel))
  (unless (emptyp channel)
    (elt (channel-timestamps channel) 0)))

(defmethod end ((channel channel))
  (unless (emptyp channel)
    (elt (channel-timestamps channel) (1- (length channel)))))


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
