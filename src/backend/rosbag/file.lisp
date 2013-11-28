;;;; file.lisp ---
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag)

(defmethod find-backend-class ((spec (eql :rosbag)))
  (find-class 'file))

(defclass file (stream-mixin
                direction-mixin
                #+no buffering-writer-mixin
                #+no last-write-time-mixin)
  ((channels        :type     list
                    :reader   get-channels
                    :accessor file-%channels
                    :documentation
                    "Stores information of the channels present in the
                     file. Entries are of the form

                       (ID NAME META-DATA ENTRIES CONNECTION-IDS)

                     .")
   (indices         :type     hash-table
                    :reader   file-%indices
                    :initform (make-hash-table :test #'eq)
                    :documentation
                    "Maps channel ids to index objects.")
   (next-channel-id :type     non-negative-integer
                    :accessor file-%next-channel-id
                    :initform 0
                    :documentation
                    "Stores the id that will be assigned to the next
                     new channel.")
   (next-chunk-id   :type     non-negative-integer
                    :accessor file-%next-chunk-id
                    :initform 0
                    :documentation
                    "Stores the id that will be assigned to the next
                     new chunk."))
  #+no (:default-initargs
   :flush-strategy (make-flush-strategy :property-limit
                                        :property :length/bytes
                                        :limit    (expt 2 25)))
  (:documentation
   "Instances of this class represent files using the 2.0 version of
    the Rosbag log file format as specified at
    http://www.ros.org/wiki/Bags/Format/2.0."))

(defmethod shared-initialize :after ((instance   file)
                                     (slot-names t)
                                     &key)
  ;; If the file is completely empty and direction includes output,
  ;; add a Rosbag header.
  #+no (when (member (backend-direction instance) '(:output :io))
    (maybe-write-header (backend-stream instance)))

  (setf (file-%channels instance) '())

  (scan (backend-stream instance) :rosbag)
  (let+ ((records (loop while (listen (backend-stream instance)) collect (unpack (backend-stream instance) :record)))
         (connections  (remove-if-not (of-type 'connection) records))
         (chunks  (remove-if-not (of-type 'chunk) records))
         ((&flet ensure-channel (topic)
            (or (find topic (file-%channels instance) :key #'second :test #'string=)
                (let* ((id (length (file-%channels instance)))
                       (c  (list id topic '() '() '())))
                  (push c (file-%channels instance))
                  c))))

         ((&flet channel-for (id)
            (or (find id (file-%channels instance) :test (rcurry #'member :test #'=) :key #'fifth)
                (error "~@<No such channel ~:D.~@:>" id)))))

    (iter (for connection each connections)
          (let ((header  (connection-data connection))
                (channel (ensure-channel (connection-topic connection))))
            (push (connection-id connection) (fifth channel))
            (setf #+later (getf (third channel) :source-name) #+later (connection-header-caller-id header)
                  (getf (third channel) :type)        (list :ros-msg (make-keyword (connection-header-type header)))
                  (getf (third channel) :format)      (unless (emptyp (connection-header-message-definition header))
                                                        (connection-header-message-definition header)))))

    (iter (for chunk each chunks)
          (iter (for message-data each (remove-if-not (of-type 'message-data) (chunk-data chunk)))
                (push (cons (let ((l (message-data-time message-data)))
                              (logior (ldb (byte 32 32) l) (ash (ldb (byte 32 0) l) 32)))
                            (message-data-data message-data))
                      (fourth (channel-for (message-data-connection message-data))))))

    (iter (for channel in (file-%channels instance))
          (setf (fourth channel) (nreverse (fourth channel)))))

  ;; TODO Scan through the Rosbag file collecting records?
  #+no (let+ (((&accessors (stream backend-stream)) instance)
         (TODO (scan stream :rosbag)))

    ;; TODO Verify presence of indices?

    ;; TODO Create indices for all channels?
    #+maybe (iter (for (id name meta-data) in channels)
                  (setf (gethash id indices)
                        (make-index id indxs chnks stream)))))

(defmethod close ((file file)
                  &key abort)
  (declare (ignore abort))

  (let+ (((&accessors-r/o (direction backend-direction)) file))
    #+TODO? (when (member direction '(:output :io))
      (map nil #'close (hash-table-values (file-%indices file))))
    (when (next-method-p)
      (call-next-method))))

(defmethod make-channel-id ((file file)
                            (name string))
  (prog1
      (file-%next-channel-id file)
    (incf (file-%next-channel-id file))))

(defmethod put-channel ((file      file)
                        (channel   integer)
                        (name      string)
                        (meta-data list))
  (let+ (((&accessors (stream   backend-stream)
                      (channels file-%channels)
                      (indices  file-%indices)) file)
         ((&plist-r/o (type          :type)
                      (source-name   :source-name   "")
                      (source-config :source-config "")
                      (format        :format        "")) meta-data)
         #+TODO (channel1 (make-instance
                    'TODO
                   :id            channel
                   :name          name
                   :type          (encode-type type)
                   :source-name   source-name
                   :source-config source-config
                   :format        format)))
    (push (list channel name meta-data) channels)

    ;; Add an index for the new channel
    #+TODO (setf (gethash channel indices)
           (make-index channel nil nil stream))

    #+TODO (pack channel1 stream)))

(defmethod get-num-entries ((file    file)
                            (channel integer))
  #+later (index-num-entries (gethash channel (file-%indices file)))
  (length (fourth (find channel (file-%channels file) :key #'first))))

(defmethod get-timestamps ((file    file)
                           (channel integer))
  #+later
  (make-instance 'timestamps
                 :entries (index-entries
                           (gethash channel (file-%indices file))))
  (mapcar (compose (lambda (l)
                     (local-time:unix-to-timestamp
                      (ldb (byte 32 32) l) :nsec (ldb (byte 32 0) l)))
                   #'car)
          (fourth (find channel (file-%channels file) :key #'first))))

(defmethod put-entry ((file      file)
                      (channel   integer)
                      (timestamp local-time:timestamp)
                      (entry     simple-array))
  #+TODO (let+ (((&accessors-r/o (buffer  backend-buffer)
                          (indices file-%indices)) file)
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
    (incf (chnk-count buffer) (+ 16 size)))) ; TODO(jmoringe): constants

(defmethod get-entry ((file    file)
                      (channel integer)
                      (index   integer))
  (cdr (nth index (fourth (find channel (file-%channels file) :key #'first))))
  #+later (let+ (((&accessors (stream backend-stream)) file)
         (index1 (gethash channel (file-%indices file))) ; TODO(jmoringe): make a method?
         (offset (index-offset index1 index))
         (length (prog2
                     (file-position stream (+ offset 12))
                     (nibbles:read-ub32/le stream)
                   (file-position stream offset)))
         (entry  (allocate-instance (find-class 'chunk-entry)))) ; TODO(jmoringe): keep instead of reallocating?
    (unpack (read-chunk-of-length (+ 16 length) stream) entry)
    (chunk-entry-entry entry)))

;;; Buffering

(defmethod make-buffer ((file     file)
                        (previous (eql nil)))
  #+TODO (make-buffer file (make-instance 'chnk
                                   :compression 0)))

#+TODO (defmethod make-buffer ((file     file)
                        (previous chnk))
  (let+ (((&accessors (next-chunk-id file-%next-chunk-id)) file))
    (reinitialize-instance previous
                           :chunk-id (incf next-chunk-id)
                           :start    (1- (ash 1 64))
                           :end      0
                           :count    0
                           :entries  (make-array 0
                                                 :adjustable   t
                                                 :fill-pointer 0))))

#+TODO (defmethod write-buffer ((file   file)
                         (buffer chnk))
  ;; We abused chnk-count to store the size of the chunk instead of
  ;; the number of entries. Correct this before writing the chunk.
  (unless (zerop (chnk-count buffer))
    (setf (chnk-count buffer) (length (chnk-entries buffer)))
    (pack buffer (backend-stream file))))

#+TODO (defmethod buffer-property ((backend file)
                            (buffer  chnk)
                            (name    (eql :length/entries)))
  (length (chnk-entries buffer)))

#+TODO (defmethod buffer-property ((backend file)
                            (buffer  chnk)
                            (name    (eql :length/bytes)))
  (chnk-count buffer))

;;; Utility functions

(defun maybe-write-header (stream)
  (when (zerop (file-length stream))
    (write-sequence +header/2.0+ stream)
    ;; TODO(jmoringe, 2013-02-15): update these later
    (pack (make-instance 'bag-header
                         :index-position   0
                         :connection-count 0
                         :chunk-count      0)
          stream)
    (file-position stream 0)))

(defun make-channel (connection)
  (check-type connection connection-header)
  (let+ (((&accessors-r/o
           (topic              connection-header-topic)
           (caller-id          connection-header-caller-id)
           (type               connection-header-type)
           (message-definition connection-header-message-definition))
          connection))
   (list TODO-ID topic
         (append (when-let ((type (decode-type type)))
                   (list :type type))
                 (list :source-name   caller-id
                       #+TODO :source-config
                       :format        message-definition)))))

#+TODO (defun make-index (channel-id indices chunks stream)
  (let ((relevant (remove channel-id indices
                          :test-not #'=
                          :key      #'indx-channel-id)))
    (make-instance 'index
                   :stream  stream
                   :channel channel-id
                   :indices relevant
                   :chunks  chunks)))

(defun encode-type (type)
  "Encode the keyword or list TYPE as a channel type string."
  #+TODO (etypecase type
    (null    "")
    (list    (let ((*package*   (find-package :keyword))
                   (*readtable* (copy-readtable)))
               (setf (readtable-case *readtable*) :invert)
               (format nil "~{~A~^:~}" type)))
    (keyword (string type))))

(defun decode-type (type)
  "Decode the channel type string TYPE as nil, a keyword of a list of
   type information."
  #+TODO (cond
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
