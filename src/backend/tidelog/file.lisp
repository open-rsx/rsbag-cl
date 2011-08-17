;;; file.lisp ---
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

(defclass file (stream-mixin
		direction-mixin
		buffering-writer-mixin)
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
   :flush?-func (rcurry #'buffer-size-> 100))
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
  (bind (((:accessors (stream          backend-stream)
		      (channels        %file-channels)
		      (indices         %file-indices)
		      (next-channel-id %file-next-channel-id)
		      (next-chunk-id   %file-next-chunk-id)) instance)
	 ((:values chans indxs chnks) (scan stream :tide)))
    ;; Sort chunks for faster lookup during index building step.
    (setf chnks           (sort (coerce chnks 'vector) #'<
				:key #'car)
	  next-chunk-id   (reduce #'max chnks
				  :initial-value 1
				  :key           #'car)
	  next-channel-id (reduce #'max chans
				  :initial-value 0
				  :key           #'chan-id)
	  channels        (map 'list #'make-channel chans))

    ;; Create indices for all channels.
    (iter (for (id name meta-data) in channels)
	  (setf (gethash id indices)
		(make-index id indxs chnks stream)))))

(defmethod close ((file file)
		  &key &allow-other-keys)
  (map nil #'close (hash-table-values (%file-indices file)))
  (when (next-method-p)
    (call-next-method)))

(defmethod make-channel-id ((file file)
			    (name string))
  (incf (%file-next-channel-id file)))

(defmethod put-channel ((file      file)
			(channel   integer)
			(name      string)
			(meta-data list))
  (bind (((:accessors (stream   backend-stream)
		      (channels %file-channels)
		      (indices  %file-indices)) file)
	 ((:plist (type          :type          "")
		  (source-name   :source-name   "")
		  (source-config :source-config "")
		  (format        :format        "")) meta-data)
	 (channel1 (make-instance
		   'chan
		   :id            channel
		   :name          name
		   :type          type
		   :source-name   source-name
		   :source-config source-config
		   :format        format)))
    (push (list channel name meta-data) channels)

    ;; Add an index for the new channel
    (setf (gethash channel indices)
	  (make-index channel nil nil stream))

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
  (bind (((:accessors-r/o (buffer  backend-buffer)
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
  (bind (((:accessors (stream backend-stream)) file)
	 (index1 (gethash channel (%file-indices file))) ;;; TODO(jmoringe): make a method?
	 (offset (index-offset index1 index))
	 (buffer (binio:make-octet-vector 16)) ;;; TODO(jmoringe): temp
	 (entry  (make-instance 'chunk-entry))) ;;; TODO(jmoringe): keep instead of reallocating
    (file-position stream offset)
    (read-sequence buffer stream)
    (setf buffer
	  (let* ((length  (binio:decode-uint32-le buffer 12))
		 (buffer1 (binio:make-octet-vector (+ length 16))))
	    (setf (subseq buffer1 0) buffer)
	    (read-sequence buffer1 stream :start 16)
	    buffer1))
    (unpack buffer entry)
    (chunk-entry-entry entry)))


;;; Buffering
;;

(defmethod make-buffer ((file     file)
			(previous (eql nil)))
  (make-buffer file (make-instance 'chnk
				   :compression 0)))

(defmethod make-buffer ((file     file)
			(previous chnk))
  (bind (((:accessors (next-chunk-id %file-next-chunk-id)) file))
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
  (unless (zerop (chnk-count buffer))
    (pack buffer (backend-stream file))))


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
	(list :type          (chan-type          chan)
	      :source-name   (chan-source-name   chan)
	      :source-config (chan-source-config chan)
	      :format        (chan-format        chan))))

(defun make-index (channel-id indices chunks stream)
  (bind ((relevant (remove channel-id indices
			   :test-not #'=
			   :key      #'indx-channel-id)))
    (make-instance 'index
		   :stream  stream
		   :channel channel-id
		   :indices relevant
		   :chunks  chunks)))

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

 ;;; TODO(jmoringe): proper counter
(defun buffer-size-> (file buffer limit)
  (declare (ignore file))
  (> (length (chnk-entries buffer)) limit))
