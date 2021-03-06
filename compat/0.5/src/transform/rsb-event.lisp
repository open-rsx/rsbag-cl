;;; rsb-event.lisp --- (De)serialization of RSB events.
;;
;; Copyright (C) 2011, 2012, 2015, 2016 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsbag.transform)

(defclass rsb-event ()
  ((wire-schema :initarg  :wire-schema
		:type     keyword
		:reader   transform-wire-schema
		:documentation
		"Stores the associated wire-schema of (de)serialized
events.")
   (holder      :reader   %transform-holder
		:initform (make-instance 'rsb.serialization:event)
		:documentation
		"Stores a data-holder instance that is reused
during (de)serialization for efficiency reasons."))
  (:default-initargs
   :wire-schema (required-argument :wire-schema))
  (:documentation
   "Instances of this transform class (de)serialize RSB events from/to
octet vectors."))

(defmethod transform-name ((transform rsb-event))
  (list (call-next-method) (transform-wire-schema transform)))

(defmethod encode ((transform rsb-event) (domain-object rsb:event))
  (let+ (((&accessors-r/o (holder %transform-holder)) transform)
	 ((&accessors-r/o (causes    rsb.serialization:event-causes)
			  (meta-data rsb.serialization:event-meta-data)) holder)
	 ((&flet+ process-timestamp (name)
	    (let ((value (rsb:timestamp domain-object name)))
	      (if value
		  (timestamp->unix-microseconds value)
		  0)))))
    ;; Prepare meta-data container.
    (reinitialize-instance meta-data
			   :create-time  (process-timestamp :create)
			   :send-time    (process-timestamp :send)
			   :receive-time (process-timestamp :receive)
			   :deliver-time (process-timestamp :deliver))
    (setf (fill-pointer (rsb.protocol:meta-data-user-infos meta-data)) 0
	  (fill-pointer (rsb.protocol:meta-data-user-times meta-data)) 0)

    ;; Add user meta-data.
    (iter (for (key value) on (rsb:event-meta-data domain-object) :by #'cddr)
	  (vector-push-extend
	   (make-instance 'rsb.protocol:user-info
			  :key   (keyword->bytes key)
			  :value (string->bytes value))
	   (rsb.protocol:meta-data-user-infos meta-data)))

    ;; Add user timestamps.
    (iter (for (key value) on (rsb:event-timestamps domain-object) :by #'cddr)
	  (unless (member key '(:create :send :receive :deliver))
	    (vector-push-extend
	     (make-instance 'rsb.protocol:user-time
			    :key       (keyword->bytes key)
			    :timestamp (timestamp->unix-microseconds value))
	     (rsb.protocol:meta-data-user-times meta-data))))

    ;; Encode causes
    (setf (fill-pointer causes) 0)
    (iter (for (origin . sequence-number) in (rsb:event-causes domain-object))
	  (vector-push-extend
	   (make-instance 'rsb.serialization:event-id
			  :sender-id       (uuid:uuid-to-byte-array origin)
			  :sequence-number sequence-number)
	   causes))

    (reinitialize-instance
     holder
     :sequence-number (rsb:event-sequence-number domain-object)
     :sender-id       (uuid:uuid-to-byte-array
		       (rsb:event-origin domain-object))
     :scope           (string->bytes
		       (rsb:scope-string (rsb:event-scope domain-object)))
     :method          (if (rsb:event-method domain-object)
			  (keyword->bytes
			   (rsb:event-method domain-object))
			  (load-time-value
			   (nibbles:make-octet-vector 0)))
     :data            (rsb:event-data domain-object))
    (pb:pack* holder)))

(defmethod decode ((transform rsb-event) (data simple-array))
  (let+ (((&flet decode-event-id (id)
	    (cons (uuid:byte-array-to-uuid
		   (rsb.serialization:event-id-sender-id id))
		  (rsb.serialization:event-id-sequence-number id))))
	 ((&accessors-r/o (holder %transform-holder)) transform)
	 ((&accessors-r/o (meta-data rsb.serialization:event-meta-data)
			  (causes    rsb.serialization:event-causes)) holder)
	 ;; Create output event.
	 (event
	  (progn
	    (setf (fill-pointer (rsb.protocol:meta-data-user-infos meta-data)) 0
		  (fill-pointer (rsb.protocol:meta-data-user-times meta-data)) 0
		  (fill-pointer causes)                                        0)
	    (pb:unpack data holder)

	    (make-instance
	     'rsb:event
	     :sequence-number   (rsb.serialization:event-sequence-number holder)
	     :origin            (uuid:byte-array-to-uuid
				 (rsb.serialization:event-sender-id holder))
	     :scope             (bytes->string
				 (rsb.serialization:event-scope holder))
	     :method            (unless (emptyp (rsb.serialization:event-method holder))
				  (bytes->keyword
				   (rsb.serialization:event-method holder)))
	     :data              (rsb.serialization:event-data holder)
	     :causes            (map 'list #'decode-event-id
				     (rsb.serialization:event-causes holder))
	     :create-timestamp? nil
	     :intern-scope?     t)))
	 ((&flet process-timestamp (name value)
	    (unless (zerop value)
	      (setf (rsb:timestamp event name)
		    (unix-microseconds->timestamp value))))))

    ;; Fill fixed timestamps.
    (process-timestamp :create  (rsb.protocol:meta-data-create-time  meta-data))
    (process-timestamp :send    (rsb.protocol:meta-data-send-time    meta-data))
    (process-timestamp :receive (rsb.protocol:meta-data-receive-time meta-data))
    (process-timestamp :deliver (rsb.protocol:meta-data-deliver-time meta-data))

    ;; Add user meta-data.
    (iter (for item each (rsb.protocol:meta-data-user-infos meta-data))
	  (setf (rsb:meta-data
		 event (bytes->keyword (rsb.protocol:user-info-key item)))
		(bytes->string (rsb.protocol:user-info-value item))))

    ;; Add user timestamps.
    (iter (for time each (rsb.protocol:meta-data-user-times meta-data))
	  (setf (rsb:timestamp
		 event (bytes->keyword (rsb.protocol:user-time-key time)))
		(unix-microseconds->timestamp
		 (rsb.protocol:user-time-timestamp time))))

    event))


;;; Utility functions
;;

(defvar *keyword-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :invert)
    readtable)
  "This readtable is used to print and read keywords. The goal is to
get a natural mapping between Lisp keywords and corresponding strings
for most cases.")

(defun timestamp->unix-microseconds (timestamp)
  "Convert the `local-time:timestamp' instance TIMESTAMP into an
integer which counts the number of microseconds since UNIX epoch."
  (+ (* 1000000 (local-time:timestamp-to-unix timestamp))
     (* 1       (local-time:timestamp-microsecond timestamp))))

(defun unix-microseconds->timestamp (unix-microseconds)
  "Convert UNIX-MICROSECONDS to an instance of
`local-time:timestamp'."
  (let+ (((&values unix-seconds microseconds)
	  (floor unix-microseconds 1000000)))
    (local-time:unix-to-timestamp
     unix-seconds :nsec (* 1000 microseconds))))

(defun string->bytes (string)
  "Converter STRING into an octet-vector."
  (sb-ext:string-to-octets string :external-format :ascii))

(defun bytes->string (bytes)
  "Convert BYTES into a string."
  (sb-ext:octets-to-string bytes :external-format :ascii))

(declaim (ftype (function (keyword) nibbles:octet-vector) keyword->bytes))

(defun keyword->bytes (keyword)
  "Convert the name of KEYWORD into an octet-vector."
  (if (find #\: (symbol-name keyword))
      (string->bytes (symbol-name keyword))
      (let ((*readtable* *keyword-readtable*))
	(string->bytes (princ-to-string keyword)))))

(declaim (ftype (function (nibbles:octet-vector) keyword) bytes->keyword))

(defun bytes->keyword (bytes)
  "Converter BYTES into a keyword."
  (if (find (char-code #\:) bytes)
      (intern (bytes->string bytes) #.(find-package :keyword))
      (let ((*package*   #.(find-package :keyword))
	    (*readtable* *keyword-readtable*))
	(read-from-string (bytes->string bytes)))))
