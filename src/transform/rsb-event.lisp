;;;; rsb-event.lisp --- (De)serialization of RSB events.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.transform)

;;; Utility functions

(defvar *keyword-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :invert)
    readtable)
  "This readtable is used to print and read keywords. The goal is to
   get a natural mapping between Lisp keywords and corresponding
   strings for most cases.")

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

(declaim (inline string->bytes bytes->string))

(defun string->bytes (string)
  "Converter STRING into a `simple-octet-vector'."
  (sb-ext:string-to-octets string :external-format :ascii))

(defun bytes->string (bytes)
  "Convert BYTES into a string."
  (sb-ext:octets-to-string bytes :external-format :ascii))

(declaim (ftype (function (keyword) (values simple-octet-vector &optional))
                keyword->bytes))

(defun keyword->bytes (keyword)
  "Convert the name of KEYWORD into a `simple-octet-vector'."
  (if (find #\: (symbol-name keyword))
      (string->bytes (symbol-name keyword))
      (let ((*readtable* *keyword-readtable*))
        (string->bytes (princ-to-string keyword)))))

(declaim (ftype (function (simple-octet-vector) (values keyword &optional))
                bytes->keyword))

(defun bytes->keyword (bytes)
  "Converter BYTES into a keyword."
  (if (find (char-code #\:) bytes)
      (values (intern (bytes->string bytes) #.(find-package '#:keyword)))
      (let ((*package*   #.(find-package '#:keyword))
            (*readtable* *keyword-readtable*))
        (values (read-from-string (bytes->string bytes))))))

(declaim (inline %reset-holder))

(defun %reset-holder (holder)
  (let+ (((&accessors-r/o (meta-data rsb.protocol:notification-meta-data)
                          (causes    rsb.protocol:notification-causes))
          holder))
    (setf (fill-pointer (rsb.protocol:event-meta-data-user-infos meta-data)) 0
          (fill-pointer (rsb.protocol:event-meta-data-user-times meta-data)) 0
          (fill-pointer causes)                                              0
          (rsb.protocol:notification-data holder)
          (load-time-value
           (nibbles:octet-vector) t)))
  holder)

;;; Transform

(defconstant +rsb-schema-name+
  (format-symbol :keyword "RSB-EVENT-~{~D~^.~}"
                 (rsbag-system:serialization-version/list)))

(defclass rsb-event ()
  ((wire-schema :initarg  :wire-schema
                :type     keyword
                :reader   transform-wire-schema
                :documentation
                "Stores the associated wire-schema of (de)serialized
                 events.")
   (holder      :reader   transform-%holder
                :initform (make-instance 'rsb.protocol:notification)
                :documentation
                "Stores a data-holder instance that is reused
                 during (de)serialization for efficiency reasons."))
  (:default-initargs
   :wire-schema (missing-required-initarg 'rsb-event :wire-schema))
  (:documentation
   "Instances of this transform class (de)serialize RSB events from/to
    octet vectors without (de)serializing payloads."))

(defmethod transform-name ((transform rsb-event))
  (list +rsb-schema-name+ (transform-wire-schema transform)))

;; TODO(jmoringe, 2012-03-04): this is a horrible hack
;; maybe the converter should supply the schema information?
(defmethod transform-format ((transform rsb-event))
  (let+ (((&structure-r/o transform- wire-schema) transform))
    (with-output-to-string (stream)
      ;; Outer serialization: RSB event serialization.
      (pbb:emit
       (load-time-value
        (pb:dependency-closure
         (pb:find-descriptor ".rsb.protocol.Notification")))
       `(:proto :stream ,stream))
      ;; Inner serialization: payload serialization (if available).
      (when (starts-with #\. (string wire-schema))
        (if-let ((descriptor (pb:find-descriptor wire-schema :error? nil)))
          (progn
            (princ #\: stream)
            (pbb:emit (pb:dependency-closure descriptor)
                      `(:proto :stream ,stream)))
          (warn "~@<Payload serialization format for wire-schema ~S is ~
                 not known. Channel format will only describe outer ~
                 event serialization; not inner payload ~
                 serialization.~@:>"
                wire-schema))))))

(defmethod encode ((transform rsb-event) (domain-object rsb:event))
  (let+ (((&accessors-r/o (holder transform-%holder)) transform)
         ((&accessors-r/o (id        rsb.protocol:notification-event-id)
                          (meta-data rsb.protocol:notification-meta-data)
                          (causes    rsb.protocol:notification-causes))
          holder)
         ((&flet process-timestamp (name)
            (if-let ((value (rsb:timestamp domain-object name)))
              (timestamp->unix-microseconds value)
              0))))
    ;; Prepare event id
    (reinitialize-instance
     id
     :sender-id       (uuid:uuid-to-byte-array
                       (rsb:event-origin domain-object))
     :sequence-number (rsb:event-sequence-number domain-object))

    ;; Prepare meta-data container.
    (reinitialize-instance meta-data
                           :create-time  (process-timestamp :create)
                           :send-time    (process-timestamp :send)
                           :receive-time (process-timestamp :receive)
                           :deliver-time (process-timestamp :deliver))
    (setf (fill-pointer (rsb.protocol:event-meta-data-user-infos meta-data)) 0
          (fill-pointer (rsb.protocol:event-meta-data-user-times meta-data)) 0)

    ;; Add user meta-data.
    (iter (for (key value) on (rsb:event-meta-data domain-object) :by #'cddr)
          (when (stringp value)
            (vector-push-extend
             (make-instance 'rsb.protocol:user-info
                            :key   (keyword->bytes key)
                            :value (string->bytes value))
             (rsb.protocol:event-meta-data-user-infos meta-data))))

    ;; Add user timestamps.
    (iter (for (key value) on (rsb:event-timestamps domain-object) :by #'cddr)
          (unless (member key rsb:*framework-timestamps*)
            (vector-push-extend
             (make-instance 'rsb.protocol:user-time
                            :key       (keyword->bytes key)
                            :timestamp (timestamp->unix-microseconds value))
             (rsb.protocol:event-meta-data-user-times meta-data))))

    ;; Encode causes
    (setf (fill-pointer causes) 0)
    (iter (for (origin . sequence-number) in (rsb:event-causes domain-object))
          (vector-push-extend
           (make-instance 'rsb.protocol:event-id
                          :sender-id       (uuid:uuid-to-byte-array origin)
                          :sequence-number sequence-number)
           causes))

    (reinitialize-instance
     holder
     :scope  (string->bytes
              (rsb:scope-string (rsb:event-scope domain-object)))
     :method (if (rsb:event-method domain-object)
                 (keyword->bytes
                  (rsb:event-method domain-object))
                 (load-time-value
                  (make-octet-vector 0)))
     :data   (rsb:event-data domain-object))
    (pb:pack* holder)))

(defmethod decode ((transform rsb-event) (data simple-array))
  (let+ (((&accessors-r/o (holder transform-%holder)) transform)
         ((&accessors-r/o (id        rsb.protocol:notification-event-id)
                          (meta-data rsb.protocol:notification-meta-data)
                          (causes    rsb.protocol:notification-causes))
          holder))
    ;; Unpack raw data into HOLDER notification instance.
    (pb:unpack data (%reset-holder holder))
    ;; Construct result event.
    (let+ (((&flet decode-event-id (id)
              (cons (uuid:byte-array-to-uuid
                     (rsb.protocol:event-id-sender-id id))
                    (rsb.protocol:event-id-sequence-number id))))
           (event
            (make-instance
             'rsb:event
             :sequence-number   (rsb.protocol:event-id-sequence-number id)
             :origin            (uuid:byte-array-to-uuid
                                 (rsb.protocol:event-id-sender-id id))
             :scope             (bytes->string
                                 (rsb.protocol:notification-scope holder))
             :method            (unless (emptyp (rsb.protocol:notification-method holder))
                                  (bytes->keyword
                                   (rsb.protocol:notification-method holder)))
             :data              (rsb.protocol:notification-data holder)
             :causes            (map 'list #'decode-event-id causes)
             :create-timestamp? nil
             :intern-scope?     t))
           ((&flet process-timestamp (name value)
              (unless (zerop value)
                (setf (rsb:timestamp event name)
                      (unix-microseconds->timestamp value))))))
      ;; Fill fixed timestamps.
      (process-timestamp :create  (rsb.protocol:event-meta-data-create-time  meta-data))
      (process-timestamp :send    (rsb.protocol:event-meta-data-send-time    meta-data))
      (process-timestamp :receive (rsb.protocol:event-meta-data-receive-time meta-data))
      (process-timestamp :deliver (rsb.protocol:event-meta-data-deliver-time meta-data))

      ;; Add user meta-data.
      (iter (for item each (rsb.protocol:event-meta-data-user-infos meta-data))
            (setf (rsb:meta-data
                   event (bytes->keyword (rsb.protocol:user-info-key item)))
                  (bytes->string (rsb.protocol:user-info-value item))))

      ;; Add user timestamps.
      (iter (for time each (rsb.protocol:event-meta-data-user-times meta-data))
            (setf (rsb:timestamp
                   event (bytes->keyword (rsb.protocol:user-time-key time)))
                  (unix-microseconds->timestamp
                   (rsb.protocol:user-time-timestamp time))))

      event)))

(defmethod print-object ((object rsb-event) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (transform-wire-schema object))))
