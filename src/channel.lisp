;;; channel.lisp --- The channel class represents a time-series of homogeneous data.
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

(cl:in-package :rsbag)

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
	      "Stores the name of the channel.")
   (meta-data :reader   channel-meta-data)
   (transform :initarg  :transform
	      :reader   channel-transform
	      :initform nil
	      :documentation
	      "Stores a transformation that should be applied to
entries when they are retrieved or stored.")
   (id        :initarg  :id
	      :reader   %channel-id
	      :documentation
	      "Stores the id of the channel.")
   (backend   :initarg  :backend
	      :reader   %channel-backend
	      :documentation
	      "Stores a reference to the backend object which
implements access to the bag to which this channel belongs."))
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
    (rsbag.backend:get-timestamps backend id)))

#+sbcl
(defmethod channel-entries ((channel channel))
  "Since CHANNEL is a sequence of its entries, nothing has to be done."
  channel)

#+sbcl
(defmethod channel-items ((channel channel))
  "Return an instance of `channel-items' which presents pairs of
timestamps and entries."
  (make-instance 'channel-items
		 :channel channel))

;;; TODO(jmoringe, 2011-12-02): entry methods are almost identical
(defmethod entry ((channel channel)
		  (index   integer)
		  &key
		  if-does-not-exist
		  (transform        (channel-transform channel)))
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel)
	 (raw (or (rsbag.backend:get-entry backend id index)
		  (ecase if-does-not-exist
		    (:error (error 'no-such-entry
				   :bag     (channel-bag channel)
				   :channel channel
				   :key     index))
		    ((nil)  nil)))))
    (if transform
	(rsbag.transform:decode transform raw)
	raw)))

(defmethod entry ((channel   channel)
		  (timestamp local-time:timestamp)
		  &key
		  if-does-not-exist)
  (bind (((:accessors-r/o (id      %channel-id)
			  (backend %channel-backend)) channel))
    (or (rsbag.backend:get-entry backend id timestamp)
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
    (rsbag.backend:put-entry backend id index raw)
    new-value))

(defmethod print-object ((object channel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S (~D)~@[ ~A~]"
	    (channel-name      object)
	    (length            object)
	    (when-let ((transform (channel-transform object)))
	      (rsbag.transform:transform-name transform)))))


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
    (rsbag.backend:get-num-entries backend id)))

#+sbcl
(defmethod sequence:make-sequence-like ((sequence channel)
					(length   integer)
					&rest args
					&key
					initial-element
					initial-contents)
  (declare (ignore initial-element initial-contents))
  (apply #'make-array length args))

#+sbcl
(defmethod sequence:elt ((channel channel)
			 (index   integer))
  (entry channel index))


;;; `channel-items' sequence class
;;

#+sbcl
(defclass channel-items (standard-object
			 sequence)
  ((channel    :initarg  :channel
	       :reader   %channel-items-channel
	       :documentation
	       "Stores the channel the items of which are used.")
   (timestamps :accessor %channel-items-timestamps
	       :documentation
	       "Stores the sequence of associated timestamps for the
entries of the channel."))
  (:default-initargs
   :channel (required-argument :channel))
  (:documentation
   "Instances of this class can be used to access the timestamps and
associated entries of a channel."))

#+sbcl
(defmethod shared-initialize :after ((instance   channel-items)
                                     (slot-names t)
                                     &key
				     channel)
  (setf (%channel-items-timestamps instance)
	(channel-timestamps channel)))

#+sbcl
(defmethod sequence:length ((items channel-items))
  (length (%channel-items-channel items)))

#+sbcl
(defmethod sequence:elt ((items channel-items)
			 (index integer))
  (bind (((:accessors-r/o (channel    %channel-items-channel)
			  (timestamps %channel-items-timestamps)) items))
    (list (elt timestamps index) (elt channel index))))
