;;;; channel.lisp --- The file class represents a channel in a TIDE log file.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

(defclass channel ()
  ((id          :initarg  :id
                :type     non-negative-integer
                :reader   channel-id)
   (name        :initarg  :name
                :type     string
                :reader   channel-name)
   (meta-data   :initarg  :meta-data
                :type     list
                :reader   channel-meta-data)
   (data-length :initarg  :data-length
                :type     (or null non-negative-integer)
                :accessor channel-data-length
                :initform nil)
   (index       :initarg  :index
                :accessor channel-index
                :initform nil))
  (:default-initargs
   :id    (missing-required-initarg 'channel :id)
   :name  (missing-required-initarg 'channel :name)
   :index (missing-required-initarg 'channel :index)))

(defmethod make-channel ((data list) &rest initargs &key)
  (apply #'make-instance 'channel data))

(defmethod make-channel ((data chan)
                         &rest initargs
                         &key
                         (make-index (missing-required-argument :make-index)))
  (let+ (((&structure-r/o chan- id name) data)
         (meta-data (append (when-let ((type (decode-type (chan-type data))))
                              (list :type type))
                            (list :source-name   (chan-source-name   data)
                                  :source-config (chan-source-config data)
                                  :format        (chan-format        data))))
         (index     (funcall make-index id)))
    (make-instance 'channel
                   :id        id
                   :name      name
                   :meta-data meta-data
                   :index     index)))
