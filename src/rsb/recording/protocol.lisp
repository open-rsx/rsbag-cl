;;;; protocol.lisp --- Protocol functions used in the rsb.recording module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.recording)

;;; Channel allocation protocol

(defgeneric channel-name-for (connection event strategy)
  (:documentation
   "Return a channel name string designating the channel within
    CONNECTION in which EVENT should be stored according to
    STRATEGY."))

(defgeneric channel-transform-for (connection event strategy)
  (:documentation
   "Derive, construct and return a transform for the channel within
    CONNECTION in which EVENT should be stored according to
    STRATEGY."))

(defgeneric channel-format-for (connection transform event strategy)
  (:documentation
   "Return a representation of the type of data/serialization
    mechanism according to which the data of EVENT, after being
    encoded by TRANSFORM, will be stored in the channel within
    CONNECTION allocated by STRATEGY."))

(defgeneric channel-meta-data-for (connection transform event strategy)
  (:documentation
   "Construct and return a meta-data plist for the channel within
    CONNECTION in which EVENT should be stored according to STRATEGY
    taking into account TRANSFORM, the associated transform for the
    channel."))

(defgeneric make-channel-for (connection event strategy)
  (:documentation
   "Return two values describing a channel in CONNECTION in which
    EVENT can be stored according to STRATEGY: 1) The meta-data plist
    for the channel 2) the transform for the channel."))

(defgeneric ensure-channel-for (connection event strategy)
  (:documentation
   "Find or make and return a channel in CONNECTION in which EVENT can
    be stored according to STRATEGY."))

;;; Default behavior

(defmethod channel-format-for ((connection t)
                               (transform  (eql nil))
                               (event      t)
                               (strategy   t))
  ;; Default behavior is to not associate a channel format.
  nil)

(defmethod channel-format-for ((connection t)
                               (transform  t)
                               (event      t)
                               (strategy   t))
  ;; Default behavior for non-nil TRANSFORM is to retrieve the channel
  ;; format from TRANSFORM.
  (transform-format transform))

;;; Channel allocation strategy class family

(service-provider:define-service strategy
  (:documentation
   "Providers implement channel selection and allocation
    strategies."))

(defgeneric make-strategy (spec &rest args)
  (:documentation
   "Return (potentially creating it first) an instance of the channel
    strategy designated by SPEC."))

(defmethod make-strategy ((spec standard-object) &rest args)
  (if args
      (apply #'reinitialize-instance spec args)
      spec))

(defmethod make-strategy ((spec symbol) &rest args)
  (if (keywordp spec)
      (apply #'service-provider:make-provider 'strategy spec
             args)
      (let ((provider (find spec (service-provider:service-providers 'strategy)
                            :key  (compose #'class-name
                                           #'service-provider:provider-class)
                            :test #'eq)))
        (apply #'service-provider:make-provider 'strategy provider
               args))))

(defmethod make-strategy ((spec class) &rest args)
  (let ((provider (find spec (service-provider:service-providers 'strategy)
                        :key  #'service-provider:provider-class
                        :test #'eq)))
    (apply #'service-provider:make-provider 'strategy provider
           args)))

(defmethod make-strategy ((spec cons) &rest args)
  (apply #'make-strategy (first spec) (append (rest spec) args)))
