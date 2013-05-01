;;;; file.lisp --- The file class represents a TIDE log file.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.backend.tidelog)

(defmethod find-backend-class ((spec (eql :tide)))
  (find-class 'file))

(defclass file (stream-mixin
		direction-mixin
		async-double-buffered-writer-mixin
		buffering-writer-mixin
		last-write-time-mixin)
  ((channels        :type     list
		    :reader   get-channels
		    :accessor %file-channels
		    :documentation
		    "Stores information of the channels present in the
file. Entries are of the form (ID NAME META-DATA).")
   (indices         :type     hash-table
		    :reader   %file-indices
		    :initform (make-hash-table :test #'eq)
		    :documentation
		    "Maps channel ids to index objects.")
   (next-channel-id :type     non-negative-integer
		    :accessor %file-next-channel-id
		    :initform 0
		    :documentation
		    "Stores the id that will be assigned to the next
new channel.")
   (next-chunk-id   :type     non-negative-integer
		    :accessor %file-next-chunk-id
		    :initform 0
		    :documentation
		    "Stores the id that will be assigned to the next
new chunk."))
  (:default-initargs
   :flush-strategy (make-flush-strategy :property-limit
					:property :length/bytes
					:limit    (expt 2 25)))
  (:documentation
   "Instances of this class represent files using the TIDE log file
format as specified at https://retf.info/svn/drafts/rd-0001.txt."))

(defmethod shared-initialize :after ((instance   file)
                                     (slot-names t)
                                     &key)
  ;; If the file is completely empty, add a TIDE header.
  (when (member (backend-direction instance) '(:output :io))
    (maybe-write-header (backend-stream instance)))

  ;; Scan through the TIDElog file collecting deserialized CHAN and
  ;; INDX blocks and the *ids and file offsets* of CHNK blocks.
  ;; Use the INDX blocks to build per-channel indices.
  (let+ (((&accessors (stream          backend-stream)
		      (direction       backend-direction)
		      (buffer          backend-buffer)
		      (channels        %file-channels)
		      (indices         %file-indices)
		      (next-channel-id %file-next-channel-id)
		      (next-chunk-id   %file-next-chunk-id)) instance)
	 ((&values chans indxs chnks) (scan stream :tide)))
    ;; Sort chunks for faster lookup during index building step.
    (setf chnks           (sort (coerce chnks 'vector) #'<
				:key #'car)
	  next-chunk-id   (1+ (reduce #'max chnks
				      :initial-value -1
				      :key           #'car))
	  next-channel-id (1+ (reduce #'max chans
				      :initial-value -1
				      :key           #'chan-id))
	  channels        (map 'list #'make-channel chans))
    (setf (chnk-chunk-id buffer) next-chunk-id)

    ;; Create indices for all channels.
    (iter (for (id name meta-data) in channels)
	  (setf (gethash id indices)
		(make-index id indxs chnks stream direction)))))

(defmethod close ((file file)
		  &key &allow-other-keys)
  (map nil #'close (hash-table-values (%file-indices file)))
  (when (next-method-p)
    (call-next-method)))

(defmethod make-channel-id ((file file)
			    (name string))
  (prog1
      (%file-next-channel-id file)
    (incf (%file-next-channel-id file))))

(defmethod put-channel ((file      file)
			(channel   integer)
			(name      string)
			(meta-data list))
  (let+ (((&accessors (stream    backend-stream)
		      (direction backend-direction)
		      (channels  %file-channels)
		      (indices   %file-indices)) file)
	 ((&plist-r/o (type          :type)
		      (source-name   :source-name   "")
		      (source-config :source-config "")
		      (format        :format        "")) meta-data)
	 (channel1 (make-instance
		   'chan
		   :id            channel
		   :name          name
		   :type          (encode-type type)
		   :source-name   source-name
		   :source-config source-config
		   :format        format)))
    (push (list channel name meta-data) channels)

    ;; Add an index for the new channel
    (setf (gethash channel indices)
	  (make-index channel nil nil stream direction))

    (pack channel1 stream)))

(defmethod get-num-entries ((file    file)
			    (channel integer))
  (index-num-entries (gethash channel (%file-indices file))))

(defmethod get-timestamps ((file    file)
			   (channel integer))
  #+sbcl
  (make-instance 'timestamps
		 :entries (index-entries
			   (gethash channel (%file-indices file))))
  #-sbcl
  #.(error "Not implemented."))

(defmethod put-entry ((file      file)
		      (channel   integer)
		      (timestamp local-time:timestamp)
		      (entry     simple-array))
  (let+ (((&accessors-r/o (buffer  backend-buffer)
			  (indices %file-indices)) file)
	 (timestamp* (timestamp->uint64 timestamp))
	 (size       (length entry))
	 (entry      (make-instance 'chunk-entry
				    :channel-id channel
				    :timestamp  timestamp*
				    :size       size
				    :entry      entry))
	 (index      (gethash channel indices)))

    ;; Update timestamps.
    (minf (chnk-start buffer) timestamp*)
    (maxf (chnk-end buffer) timestamp*)

    ;; (incf (chnk-count buffer))
    (vector-push-extend entry (chnk-entries buffer))

    ;; Update index. Abuse chnk-count for tracking offsets
    (put-entry index timestamp* (chnk-count buffer) (chnk-chunk-id buffer))
    (incf (chnk-count buffer) (+ 16 size)))) ;;; TODO(jmoringe): constants

(defmethod get-entry ((file    file)
		      (channel integer)
		      (index   integer))
  (let+ (((&accessors (stream backend-stream)) file)
	 (index1 (gethash channel (%file-indices file))) ;;; TODO(jmoringe): make a method?
	 (offset (index-offset index1 index))
	 (length (prog2
		     (file-position stream (+ offset 12))
		     (nibbles:read-ub32/le stream)
		   (file-position stream offset)))
	 (entry  (allocate-instance (find-class 'chunk-entry)))) ;;; TODO(jmoringe): keep instead of reallocating?
    (unpack (read-chunk-of-length (+ 16 length) stream) entry)
    (chunk-entry-entry entry)))


;;; Buffering
;;

(defmethod make-buffer ((file     file)
			(previous (eql nil)))
  (make-buffer file (make-instance 'chnk
				   :compression 0)))

(defmethod make-buffer ((file     file)
			(previous chnk))
  (let+ (((&accessors (next-chunk-id %file-next-chunk-id)) file))
    (reinitialize-instance previous
			   :chunk-id (incf next-chunk-id)
			   :start    (1- (ash 1 64))
			   :end      0
			   :count    0
			   :entries  (make-array 0
						 :adjustable   t
						 :fill-pointer 0))))

(defmethod write-buffer ((file   file)
			 (buffer chnk))
  ;; We abused chnk-count to store the size of the chunk instead of
  ;; the number of entries. Correct this before writing the chunk.
  (unless (zerop (chnk-count buffer))
    (setf (chnk-count buffer) (length (chnk-entries buffer)))
    (pack buffer (backend-stream file))

    ;; For the sake of conservative garbage collectors, we deference
    ;; as much as possible here. On SBCL we even garbage collect
    ;; explicitly.
    (map-into (chnk-entries buffer)
              (lambda (entry)
                (setf (chunk-entry-entry entry) (nibbles:octet-vector))
                nil)
              (chnk-entries buffer))
    #+sbcl (sb-ext:gc)))

(defmethod buffer-property ((backend file)
			    (buffer  chnk)
			    (name    (eql :length/entries)))
  (length (chnk-entries buffer)))

(defmethod buffer-property ((backend file)
			    (buffer  chnk)
			    (name    (eql :length/bytes)))
  (chnk-count buffer))


;;; Utility functions
;;

(defun maybe-write-header (stream)
  (when (zerop (file-length stream))
    (pack (make-instance 'tide
			 :version-major +format-version-major+
			 :version-minor +format-version-minor+
			 :num-channels  0
			 :num-chunks    0)
	  stream)
    (file-position stream 0)))

(defun make-channel (chan)
  (list (chan-id chan)
	(chan-name chan)
	(append (when-let ((type (decode-type (chan-type chan))))
		  (list :type type))
		(list :source-name   (chan-source-name   chan)
		      :source-config (chan-source-config chan)
		      :format        (chan-format        chan)))))

(defun make-index (channel-id indices chunks stream direction)
  (let ((relevant (remove channel-id indices
			  :test-not #'=
			  :key      #'indx-channel-id)))
    (make-instance 'index
		   :stream    stream
		   :direction direction
		   :channel   channel-id
		   :indices   relevant
		   :chunks    chunks)))

(defun encode-type (type)
  "Encode the keyword or list TYPE as a channel type string."
  (etypecase type
    (null    "")
    (list    (let ((*package*   (find-package :keyword))
		   (*readtable* (copy-readtable)))
	       (setf (readtable-case *readtable*) :invert)
	       (format nil "~{~A~^:~}" type)))
    (keyword (string type))))

(defun decode-type (type)
  "Decode the channel type string TYPE as nil, a keyword of a list of
type information."
  (cond
    ((emptyp type)
     nil)
    ((find #\: type)
     (let+ (((class-name &rest arg-strings) (split-sequence #\: type))
	    (*package*   (find-package :keyword))
	    (*readtable* (copy-readtable))
	    (class       (progn
			   (setf (readtable-case *readtable*) :invert)
			   (read-from-string class-name)))
	    (args        (map 'list #'read-from-string arg-strings)))
       (cons class args)))
    (t
     (make-keyword type))))

(declaim (ftype (function ((unsigned-byte 32)
			   (array (cons (unsigned-byte 32) (unsigned-byte 64)) (*))
			   &optional
			   non-negative-fixnum
			   non-negative-fixnum)
			  (unsigned-byte 64))
		%chunk-id->offset))

(defun %chunk-id->offset (id index
			  &optional
			  (start 0)
			  (end   (length index)))
  (let* ((pivot  (ash (+ start end) -1))
	 (pivot* (car (aref index pivot))))
    (cond
      ((< pivot* id)
       (%chunk-id->offset id index pivot end))
      ((> pivot* id)
       (%chunk-id->offset id index start pivot))
      (t (cdr (aref index pivot))))))
