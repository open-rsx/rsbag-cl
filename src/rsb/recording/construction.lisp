;;;; construction.lisp --- Construction of channel <-> events recording connections.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.recording)

;;; Recording with RSB listener source

(defmethod events->bag ((source rsb:listener)
                        (dest   bag)
                        &key
                        (timestamp        :send)
                        (channel-strategy :scope-and-type)
                        &allow-other-keys)
  (make-instance 'recording-participant-channel-connection
                 :bag       dest
                 :endpoint  source
                 :timestamp timestamp
                 :strategy  (make-strategy channel-strategy)))

(defmethod events->bag ((source puri:uri)
                        (dest   bag)
                        &rest args
                        &key
                        (transports '((t :expose (:rsb.transport.wire-schema)
                                       rsb:&inherit)))
                        (filters    nil filters-supplied?))
  (let ((listener (apply #'rsb:make-participant :listener source
                         :transports transports
                         :converters '((t . :fundamental-null))
                         (when filters-supplied?
                           (list :filters filters)))))
    (apply #'events->bag listener dest
           (remove-from-plist args :transports :filters))))

(defmethod events->bag
    ((source sequence)
     (dest   bag)
     &rest args &key
     (error-policy          nil error-policy-supplied?)
     (introspection-survey? t   introspection-survey?-supplied?)
     (start?                t))
  (let* ((args/channel (remove-from-plist
                        args :error-policy :introspection-survey? :start?))
         (connection   (apply #'make-instance 'recording-bag-connection
                              :bag      dest
                              :channels (map 'list
                                             (lambda (source)
                                               (apply #'events->bag source dest
                                                      args/channel))
                                             source)
                              (append
                               (when introspection-survey?-supplied?
                                 (list :introspection-survey?
                                       introspection-survey?))
                               (when error-policy-supplied?
                                 (list :error-policy error-policy))))))
    (when start?
      (start connection))
    connection))

;;; Recording without source

(defmethod events->bag ((source null)
                        (dest   bag)
                        &key
                        (error-policy     nil error-policy-supplied?)
                        (timestamp        :send)
                        (channel-strategy :scope-and-type)
                        &allow-other-keys)
  (apply #'make-instance 'recording-bag-connection
         :bag      dest
         :channels (list (make-instance 'recording-channel-connection
                                        :bag       dest
                                        :timestamp timestamp
                                        :strategy  (make-strategy channel-strategy)))
         (when error-policy-supplied?
           (list :error-policy error-policy))))
