;;; index.lisp ---
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

(in-package :rsbag.backend.tidelog)

(defclass index ()
  ((channel :initarg  :channel
	    :type     non-negative-integer
	    :reader   index-channel
	    :documentation
	    "")
   (entries :type     vector
	    :accessor index-entries
	    :documentation
	    "")
   (stream  :initarg  :stream
	    :type     stream
	    :reader   index-stream
	    :documentation
	    "")
   (buffer  :type     indx
	    :reader   index-buffer
	    :initform (make-instance
		       'indx
		       :count   0
		       :entries (make-array 1024
					    :adjustable   t
					    :fill-pointer 0))
	    :documentation
	    ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod initialize-instance :after ((instance index)
                                       &key
				       indices
				       chunks)
  (setf (index-entries instance)
	(make-entries indices chunks)
	(indx-channel-id (index-buffer instance))
	(index-channel instance)))

(defmethod close ((index index)
		  &key &allow-other-keys)
  (write-buffer index (index-buffer index)))

(defmethod index-num-entries ((index index))
  (ash (length (index-entries index)) -1))

(defmethod index-offset ((index index)
			 (index1 integer))
  (aref (index-entries index) (1+ (* 2 index1))))

(defmethod index-offset ((index     index)
			 (timestamp local-time:timestamp))
  (%timestamp->index timestamp (index-entries index)))

(defmethod put-entry ((index     index)
		      (timestamp integer)
		      (offset    integer)
		      (chunk-id  integer))
  (bind (((:accessors-r/o (buffer  index-buffer)
			  (entries index-entries)) index))
    ;; Add to entries.
    (vector-push-extend timestamp entries)
    (vector-push-extend offset    entries)

    ;; Update index block.
    (incf (indx-count buffer))
    (vector-push-extend
     (make-instance 'index-entry
		    :chunk-id  chunk-id
		    :timestamp timestamp
		    :offset    offset)
     (indx-entries buffer))))


;;; Buffereing
;;

(defmethod write-buffer ((index  index)
			 (buffer indx))
  (unless (zerop (indx-count buffer))
    (pack buffer (index-stream index)))
  (setf (indx-count buffer)                  0
	(fill-pointer (indx-entries buffer)) 0))


;;; Utility functions
;;

(defun make-entries (indices chunks)
  (bind ((result (make-array (* 2 (reduce #'+ indices
					  :key #'indx-count))
			     :element-type '(unsigned-byte 64)
			     :adjustable   t
			     :fill-pointer 0))
	 ((:flet add-offset! (entry))
	  (bind (((:accessors-r/o
		   (timestamp    index-entry-timestamp)
		   (chunk-id     index-entry-chunk-id)
		   (inner-offset index-entry-offset)) entry)
		 (outer-offset (%chunk-id->offset chunk-id chunks)))
	    (vector-push timestamp result)
	    (vector-push (+ outer-offset 12 25 inner-offset) result)))) ;;; TODO(jmoringe):  get rid of the constants
    (iter (for index each indices)
	  (map nil #'add-offset! (indx-entries index)))
    result))

(declaim (ftype (function ((unsigned-byte 64)
			   (array (unsigned-byte 64) (*))
			   &optional
			   non-negative-fixnum
			   non-negative-fixnum)
			  (unsigned-byte 64))
		%timestamp->index))

(defun %timestamp->index (timestamp index
			  &optional
			  (start 0)
			  (end   (length index)))
  (let* ((pivot  (ash (+ start end) -1))
	 (pivot* (aref index (* 2 pivot))))
    (cond
      ((< pivot* timestamp)
       (%timestamp->index timestamp index pivot end))
      ((> pivot* timestamp)
       (%timestamp->index timestamp index start pivot))
      (t (aref index (1+ (* 2 pivot)))))))