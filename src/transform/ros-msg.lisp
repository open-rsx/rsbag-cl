;;;; ros-msg.lisp --- (De)serialization of ROS messages.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.transform)

(defmethod make-transform ((spec (eql :ros-msg))
                           &rest args)
  "Return an instance of `ros-msg'."
  (let+ (((type &key format &allow-other-keys) args))
    (make-instance 'ros-msg :type type :format format)))

(defclass ros-msg ()
  ((type   :initarg  :type ; TODO store parsed type
           :type     keyword #+maybe string
           :reader   transform-type
           :documentation
           "")
   (format :initarg  :format
           :type     (or null string)
           :reader transform-format
           :documentation
           ""))
  (:default-initargs
   :type   (missing-required-initarg 'ros-msg :type)
   :format (missing-required-initarg 'ros-msg :format))
  (:documentation
   "Instances of this transform class try to encode and decode TODO"))

(defmethod shared-initialize :after ((instance   ros-msg)
                                     (slot-names t)
                                     &key)
  (let+ (((&accessors-r/o (type   transform-type)
                          (format transform-format)) instance))
    (if (or (not format) (emptyp format))
        (warn "~@<No message definition is available for ~S; returning ~
               raw data.~@:>"
              type)
        (let+ (((package-name . message-name) (rs.ros:parse-type-name (string type)))
               (repository (make-instance 'rs.m.d::base-repository)))
          (handler-bind ((warning #'muffle-warning))
            (rs.f:process `(:ros-msg/with-dependencies :package ,package-name
                                                       :name    ,message-name)
                          format
                          `(:model :repository ,repository)))
          (let ((structure (rs.m.d:lookup repository :structure `(:absolute ,package-name ,message-name))))
            (let+ (((&labels rec (thing &optional (depth 0))
                      (format t "~V@T> ~A~%" depth thing)
                      (typecase thing
                        (rs.m.d:base-field
                         (rec (rs.m.d:type1 thing) (1+ depth)))
                        (rs.m.d:base-structure
                         (mapc (rcurry #'rec (1+ depth)) (rs.m.d:contents thing :field))
                         (rs.b:generate thing :class :lisp/compiled))
                        (rs.m.d:base-array
                         (rec (rs.m.d:element-type thing) (1+ depth)))))))
              (rec structure))
            (rs.b:generate structure :ros-msg-unpack/method :lisp/compiled))))))

(defmethod transform-name ((transform ros-msg))
  (list :ros-msg (transform-type transform)))

;; TODO(jmoringe, 2012-03-04): this is a horrible hack
;; maybe the converter should supply the schema information?
#+no (defmethod transform-format ((transform rsb-event))
  #+later (let+ (((&accessors-r/o (wire-schema transform-wire-schema)) transform))
    )
  "")

(defmethod encode ((transform ros-msg) (domain-object t))
  domain-object
  #+later (let+ (((&accessors-r/o (holder %transform-holder)) transform)
         ((&accessors-r/o (id        rsb.protocol:notification-event-id)
                          (meta-data rsb.protocol:notification-meta-data)
                          (causes    rsb.protocol:notification-causes)) holder)
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

(defmethod decode ((transform ros-msg) (data simple-array))
  (let* ((name (find-symbol (string-upcase
                             (rosetta:normalize-name
                              (cdr (rs.ros:parse-type-name (string (transform-type transform)))))) ; TODO hack
                            :cl-user))
         (class (when name
                  (or (find-class name nil)
                      (when-let ((name (find-symbol (format nil "~A1" name) :cl-user))) ; TODO hack
                        (find-class name nil))))))
    (if class
       (handler-case
           (rs.s:unpack :ros-msg data class)
         (serious-condition (condition)
           (error (princ-to-string condition)))) ; TODO
       data)))

(defmethod print-object ((object ros-msg) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (transform-type object))))
