;;; serialized.lisp --- Serialized view on data from multiple channels.
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

(in-package :rsbag.view)


;;; Construction methods
;;

(defmethod make-serialized-view ((sequences bag)
				 &key
				 (selector #'identity))
  "Create a serialized view for the channels of a bag."
  (make-serialized-view (bag-channels sequences)
			:selector selector))

(defmethod make-serialized-view ((sequences sequence)
				 &key
				 (selector #'identity))
  (let* ((transformed (map 'list selector sequences))
	 (key         (if transformed
			  (%make-key-function (first transformed))
			  #'identity)))
    (make-instance 'serialized
		   :sequences transformed
		   :key       key)))


;;; Key creation methods
;;

(defmethod %make-key-function ((sequence sequence))
  "When SEQUENCE is just a `sequence', we assume it consists of
timestamps."
  #'(lambda (sequence iterator limit from-end)
      (unless (sequence:iterator-endp sequence iterator limit from-end)
	(sequence:iterator-element sequence iterator))))

(defmethod %make-key-function ((sequence channel))
  "When SEQUENCE is a `channel', we can use timestamps as keys by
using the index of the iterator and looking up the corresponding
timestamp in `channel-timestamps'."
  #'(lambda (sequence iterator limit from-end)
      (unless (sequence:iterator-endp sequence iterator limit from-end)
	(sequence:elt
	 (channel-timestamps sequence)
	 (sequence:iterator-index sequence iterator)))))

(defmethod %make-key-function ((sequence channel-items))
  "When SEQUENCE is of type `channel-items', we can use the index of
the iterator and look up the corresponding timestamp in the timestamp
sequence."
  #'(lambda (sequence iterator limit from-end)
      (unless (sequence:iterator-endp sequence iterator limit from-end)
	(sequence:elt
	 (rsbag::%channel-items-timestamps sequence)
	 (sequence:iterator-index sequence iterator)))))


;;; `serialized' class
;;

(defclass serialized (multi-sequence-view-mixin
		      elt-via-iterator-mixin
		      sequence)
  ((compare :initarg  :compare
	    :type     function
	    :accessor view-compare
	    :initform #'local-time:timestamp<
	    :documentation
	    "Stores a function that is used to compare keys extracted
from iterator states in order to decide which iterator has to be
stepped.")
   (key     :initarg  :key
	    :type     function
	    :accessor view-key
	    :documentation
	    "Stores a function that extracts keys from iterator states
which are used to decide which iterator has to be stepped."))
  (:default-initargs
   :key (required-argument :key))
  (:documentation
   "Instances of this class provide the data of multiple channels as a
single sequence in which items from different channels are serialized
according to their timestamps."))

(defmethod sequence:length ((view serialized))
  ;; The number of events is the sums of the numbers of events of
  ;; individual channels.
  (reduce #'+ (view-sequences view) :key #'length))

(defmethod sequence:make-simple-sequence-iterator ((view serialized)
						   &key
						   (start   0)
						   end
						   from-end)
  (bind (((:accessors-r/o (sequences view-sequences)
			  (compare   view-compare)
			  (key       view-key)) view)
	 ((:flet make-iterator (sequence))
	  (bind (((:values iterator limit from-end)
		  (sequence:make-simple-sequence-iterator
		   sequence :from-end from-end)))
	    (list (funcall key sequence iterator limit from-end)
		  sequence iterator limit from-end)))
	 (iterator (make-instance
		    'serialized-iterator
		    :iterators (map 'list #'make-iterator sequences)
		    :compare   compare)))
    (iter (repeat start)
	  (setf iterator (sequence:iterator-step view iterator from-end)))
    (values iterator end from-end)))


;;; `serialized-iterator' class
;;

(defclass serialized-iterator (multi-sequence-iterator-mixin)
  ((current :initarg  :current
	    :accessor %iterator-current
	    :documentation
	    "Stores the iterator that holds the current element and
has to be stepped in order to step in the serialized view."))
  (:documentation
   "Instances of this class perform iterations through sequences that
are serialized views on multiple sequences."))

(defmethod shared-initialize :after ((instance   serialized-iterator)
                                     (slot-names t)
                                     &key
				     compare)
  (setf (%iterator-current instance)
	(%next-iterator (%iterator-iterators instance) compare)))

(defmethod sequence:iterator-endp ((sequence serialized)
				   (iterator serialized-iterator)
				   (limit    t)
				   (from-end t))
  (or (null (first (%iterator-current iterator)))
      (call-next-method)))

(defmethod sequence:iterator-step ((sequence serialized)
				   (iterator serialized-iterator)
				   (from-end t))
  (bind (((:accessors-r/o (compare view-compare)
			  (key     view-key)) sequence)
	 (current (%iterator-current iterator))
	 ((_ sequence* iterator* _ from-end*) current))
    (declare (type function key))
    (setf (third current)
	  (sequence:iterator-step sequence* iterator* from-end*)
	  (first current)
	  (apply key (rest current))
	  (%iterator-current iterator)
	  (%next-iterator (%iterator-iterators iterator) compare)))
  iterator)

(defmethod sequence:iterator-element ((sequence serialized)
				      (iterator serialized-iterator))
  (bind (((:accessors-r/o (current %iterator-current)) iterator))
    (sequence:iterator-element (second current) (third current))))


;;; Utility functions
;;

(declaim (inline %next-iterator)
	 (ftype (function (list function) t) %next-iterator))

(defun %next-iterator (iterators compare)
  "Return the iterator in ITERATORS that should be used to retrieve
the next element of the serialized sequence or nil."
  (when iterators
    (reduce (rcurry #'%iterator-min compare) iterators)))

(declaim (ftype (function (list list function) t) %iterator-min))

(defun %iterator-min (left right compare)
  (cond
    ((null (first left))
     right)
    ((null (first right))
     left)
    ((funcall compare (first left) (first right))
     left)
    (t
     right)))
