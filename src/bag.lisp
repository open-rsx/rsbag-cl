;;; bag.lisp --- The bag class represent data channels stored in a file.
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

(defclass bag ()
  ((direction :initarg  :direction
	      :type     direction
	      :reader   bag-direction
	      :documentation
	      "Stores the direction of the bag.")
   (backend   :initarg  :backend
	      :reader   %bag-backend
	      :documentation
	      "Stores an object which is responsible for accessing the
stream associated to this bag.")
   (transform :initarg  :transform
	      :type     transform-spec
	      :reader   bag-transform
	      :initform &from-source
	      :documentation
	      "Stores a specification for transformations that should
be associated with channels of the bag. See type `transform-spec'.")
   (channels  :type     hash-table
	      :reader   %bag-channels
	      :initform (make-hash-table :test #'equal)
	      :documentation
	      "Stores a mapping of channel names to `channel'
instances."))
  (:default-initargs
   :direction (required-argument :direction)
   :backend   (required-argument :backend))
  (:documentation
   "Instances of this class represent a log file. A log file consists
of named channels which can be retrieved using the `bag-channels' and
`bag-channel' methods and modified using the `(setf bag-channel)'
method. "))

(defmethod shared-initialize :after ((instance   bag)
                                     (slot-names t)
                                     &key)
  (bind (((:accessors-r/o (backend   %bag-backend)
			  (transform bag-transform)
			  (channels  %bag-channels)) instance)
	 ((:flet make-transform (name meta-data id))
	  (%make-channel-transform instance name meta-data
				   :id   id
				   :spec transform)))
    (iter (for (id name meta-data) in (rsbag.backend:get-channels backend))
	  (setf (gethash name channels)
		(%make-channel instance name meta-data
			       (make-transform name meta-data id)
			       :id id)))))

(defmethod close ((bag bag)
		  &key &allow-other-keys)
  (close (%bag-backend bag)))

(defmethod bag-channels ((bag bag))
  (hash-table-values (%bag-channels bag)))

(defmethod bag-channel ((bag bag) (name string)
			&key
			(if-does-not-exist :error))
  (or (gethash name (%bag-channels bag))
      (ecase if-does-not-exist
	(:error (error 'no-such-channel
		       :bag  bag
		       :name name))
	((nil)  nil))))

(defmethod (setf bag-channel) :before ((new-value t)
				       (bag       t)
				       (name      t)
				       &key &allow-other-keys)
  (when (eq (bag-direction bag) :input)
    (error 'read-only-bag
	   :bag bag)))

(defmethod (setf bag-channel) ((new-value list)
			       (bag       bag)
			       (name      string)
			       &key
			       (if-exists :error)
			       (transform (%make-channel-transform
					   bag name new-value
					   :spec (bag-transform bag))))
  ;; If a channel named NAME already exists, apply IF-EXISTS policy.
  (when-let ((channel (gethash name (%bag-channels bag))))
    (ecase if-exists
      (:error     (error 'channel-exists
			 :bag     bag
			 :channel channel))
      (:supersede (error "Superseding not implemented")))) ;;; TODO(jmoringe): implement

  ;; If NEW-VALUE does not have a type, but TRANSFORM is non-nil,
  ;; augment the meta-data with TRANSFORM's type. Make a channel
  ;; instance and store it.
  (bind (((:accessors-r/o (channels %bag-channels)
			  (backend  %bag-backend)) bag)
	 (meta-data (if (and transform (not (getf new-value :type)))
			(append (list :type (rsbag.transform:transform-name transform))
				new-value)
			new-value))
	 (channel   (%make-channel bag name meta-data transform)))
    (rsbag.backend:put-channel
     backend (%channel-id channel) name meta-data)
    (setf (gethash name channels) channel)))

(defmethod print-object ((object bag) stream)
  (bind (((:accessors-r/o (direction bag-direction)
			  (channels  %bag-channels)) object))
   (print-unreadable-object (object stream :type t :identity t)
     (format stream "~:[-~;r~]~:[-~;w~] (~D)"
	     (member direction '(:input :io))
	     (member direction '(:output :io))
	     (hash-table-count channels)))))


;;; Time range protocol
;;

(macrolet ((define-bound-method (name comparator)
	     `(defmethod ,name ((bag bag))
		(flet ((safe-compare (left right)
			 (cond
			   ((not left)               right)
			   ((not right)              left)
			   ((,comparator left right) left)
			   (t                        right))))
		  (reduce #'safe-compare (bag-channels bag)
			  :key           (function ,name)
			  :initial-value nil)))))
  (define-bound-method start local-time:timestamp<)
  (define-bound-method end   local-time:timestamp>))


;;;
;;

(defmethod %channel-class ((bag bag))
  (find-class 'channel))

(defmethod %make-channel ((bag       bag)
			  (name      string)
			  (meta-data list)
			  (transform t)
			  &key
			  id)
  (bind (((:accessors-r/o (backend       %bag-backend)
			  (channel-class %channel-class)) bag))
    (make-instance channel-class
		   :bag       bag
		   :name      name
		   :transform transform
		   :id        (or id (rsbag.backend:make-channel-id
				      backend name))
		   :meta-data meta-data
		   :backend   backend)))

(defmethod %make-channel-transform ((bag       bag)
				    (name      string)
				    (meta-data list)
				    &key
				    id
				    spec)
  (declare (ignore id))

  (bind (((:plist type) meta-data)
	 ((:flet parse-type ())
	  (typecase type
	    (null (list nil))
	    (list type)
	    (t    (ensure-list type))))
	 ((class-name &rest args)
	  (etypecase spec
	    ;; No spec - derive everything from TYPE.
	    (transform-spec/default
	     (parse-type))

	    ;; Spec with :FROM-SOURCE - append spec to information
	    ;; derived from TYPE.
	    (transform-spec/augment
	     (append (parse-type) (rest spec)))

	    ;; Spec without :FROM-SOURCE - ignore TYPE and use
	    ;; supplied spec unmodified.
	    (transform-spec/full
	     spec))))
    (when class-name
      (handler-case ;;; TODO(jmoringe): add :error? nil in find-transform-class
	  (apply #'rsbag.transform:make-transform class-name args)
	(rsbag.transform:no-such-transform-class (condition)
	  (declare (ignore condition))
	  nil)))))
