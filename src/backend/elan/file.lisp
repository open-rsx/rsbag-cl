;;; file.lisp --- Elan file format support.
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

(cl:in-package :rsbag.backend.elan)

(defmethod find-backend-class ((spec (eql :elan)))
  (find-class 'file))

(defmethod find-backend-class ((spec (eql :eaf)))
  (find-class 'file))

(defclass file (stream-mixin
		direction-mixin)
  ((channels        :type     list
		    :reader   get-channels
		    :accessor %file-channels
		    :initform nil
		    :documentation
		    "Stores information of the channels (or tiers,
rather) present in the file. Entries are of the form

  (ID NAME META-DATA)

where ID is the numeric id of the channel, NAME is the name as string
and META-DATA is a plist of additional data associated to the
channel.")
   (data            :type     hash-table
		    :reader   %file-data
		    :initform (make-hash-table :test #'eq)
		    :documentation
		    "Maps channel ids to index objects.")
   (document        :type     stp:document
		    :accessor %file-document
		    :documentation
		    "Stores the `stp:document' instance which contains
the DOM representation of the file. The document is not updated
continuously but only on write-back.")
   (next-channel-id :type     non-negative-integer
		    :accessor %file-next-channel-id
		    :initform 0
		    :documentation
		    "Stores the id that will be assigned to the next
new channel."))
  (:documentation
   "Instances of this class represent Elan eaf-files. All data is
serialized and written or read and deserialized when the file is
written or read respectively."))

(defmethod shared-initialize :after ((instance   file)
                                     (slot-names t)
                                     &key)
  (bind (((:accessors (direction       backend-direction)
		      (stream          backend-stream)
		      (document        %file-document)
		      (channels        %file-channels)
		      (data            %file-data)
		      (next-channel-id %file-next-channel-id)) instance)
	 ((date urls time-slots tiers)
	  (cond
	    ((listen stream)
	     (setf document (cxml:parse stream (stp:make-builder)))
	     (xloc:xml-> document 'file/list))
	    ((member direction '(:output :io))
	     (setf document (stp:make-document (stp:make-element "ANNOTATION_DOCUMENT")))
	     (list (local-time:now) nil nil nil))
	    (t
	     (error 'in"~@<EAF file is empty: ~S~@:>"
		    stream))))
	 (base (timestamp->millisecs date))
	 ((:flet resolve (id))
	  (cdr (assoc id time-slots :test #'string=))))

    (setf (%file-document instance) document) ;;; TODO(jmoringe):

    ;; Add video channels.
    (iter (for url in urls)
	  (for i :from 0)
	  (let* ((name (format nil "video~D" i))
		 (id   (make-channel-id instance name)))
	    (push (list id name '(:type (:rsb-event :|.rst.vision.Image|)))
		  channels)))

    ;; Add annotation channels.
    (iter (for (name content) in tiers)
	  (let ((id (make-channel-id instance name)))
	    (push (list id name '(:type :utf-8-string)) channels)
	    (setf (gethash id data)
		  (map 'list (curry #'apply
				    (lambda (start end datum)
				      (list (+ base (resolve start))
					    (+ base (resolve end))
					    datum)))
		       content))))))

(defmethod close ((file file)
		  &key &allow-other-keys)
  "TODO(jmoringe): document"
  (bind (((:accessors-r/o (stream   backend-stream)
			  (document %file-document)
			  (channels %file-channels)
			  (data     %file-data)) file)
	 (time-slots           (make-hash-table :test #'eql))
	 (current-time-slot-id 0)
	 ((:flet time-slot (timestamp))
	  (or (gethash timestamp time-slots)
	      (setf (gethash timestamp time-slots)
		    (format nil "ts~D" (incf current-time-slot-id)))))
	 (tiers (iter (for (id name meta-data) in channels)
		      (for entries             next (gethash id data))
		      (collect (list name (map 'list (curry #'apply #'(lambda (start end datum)
									(list (time-slot start)
									      (time-slot end)
									      datum)))
					       (sort (coerce entries 'vector) #'< :key #'first))))))
	 (time-slots (iter (for (timestamp id) in-hashtable time-slots)
			   (collect (cons id timestamp))))
	 (foo (list (local-time:now) nil time-slots tiers))) ;;; TODO(jmoringe, 2011-12-01): media stuff
    (xloc:->xml foo document 'file/list)
    (file-position stream 0)
    (stp:serialize document (cxml:make-octet-stream-sink
			     (make-broadcast-stream stream)
			     :indentation 2)))
  (when (next-method-p)
    (call-next-method)))

(defmethod make-channel-id ((backend file)
			    (name    string))
  (prog1
      (%file-next-channel-id backend)
    (incf (%file-next-channel-id backend))))

(defmethod put-channel ((backend   file)
			(channel   integer)
			(name      string)
			(meta-data list))
  (bind (((:plist (type _ :utf-8-string)) meta-data)
	 ((:accessors (channels %file-channels)) backend))
    (unless (eq type :utf-8-string)
      (error "~@<Cannot handle channel type ~S; only ~S is supported.~@:>"
	     type :utf-8-string))

    (push (list channel name (append (remove-from-plist meta-data :type)
				     (list :type type)))
	  channels)))

(defmethod get-num-entries ((file    file)
			    (channel integer))
  (length (gethash channel (%file-data file))))

(defmethod get-timestamps ((file    file)
			   (channel integer))
  (map 'list (compose #'millisecs->timestamp #'car)
       (gethash channel (%file-data file))))

(defmethod put-entry ((file      file)
		      (channel   integer)
		      (timestamp local-time:timestamp)
		      (entry     string))
  (bind (((:accessors-r/o (data  %file-data)) file)
	 (timestamp* (timestamp->millisecs timestamp)))
    (push (list timestamp* timestamp* entry)
	  (gethash channel data))))

(defmethod get-entry ((file    file)
		      (channel integer)
		      (index   integer))
  (third (nth index (gethash channel (%file-data file)))))


;;;
;;

(defun millisecs->timestamp (value)
  (bind (((:values secs msecs) (truncate value 1000)))
    (local-time:unix-to-timestamp secs :nsec (* 1000000 msecs))))

(defun timestamp->millisecs (value)
  (bind (((:accessors-r/o (secs  local-time:timestamp-to-unix)
			  (nsecs local-time:nsec-of)) value)
	 (msecs (truncate nsecs 1000000)))
    (+ (* 1000 secs) msecs)))

#+example
(with-input-from-file (stream "/homes/jmoringe/code/asya/SSL_JI_int.eaf")
  (make-instance 'file :stream stream))
