;;;; construction.lisp --- Construction of channel <-> events connections.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; RSB events -> bag

(defmethod events->bag ((source string)
                        (dest   bag)
                        &rest args &key)
  (apply #'events->bag (puri:parse-uri source) dest args))

(defun %events->bag/streamish (source dest
                               &rest args
                               &key
                               (if-exists      :error if-exists-supplied?)
                               backend
                               (flush-strategy nil)
                               (bag-class      'synchronized-bag)
                               &allow-other-keys)
  (let ((bag (apply #'open-bag dest
                    :bag-class bag-class
                    :direction :output
                    (append (when if-exists-supplied?
                              (list :if-exists if-exists))
                            (when backend
                              (list :backend backend))
                            (when flush-strategy
                              (list :flush-strategy flush-strategy))))))
    (apply #'events->bag source bag
           (remove-from-plist args :backend :flush-strategy :bag-class))))

(macrolet ((define-open-bag-method (type)
             `(progn
                (defmethod events->bag ((source string)
                                        (dest   ,type)
                                        &rest args &key)
                  (apply #'events->bag (list source) dest args))

                (defmethod events->bag ((source t)
                                        (dest   ,type)
                                        &rest args &key)
                  (apply #'%events->bag/streamish source dest args)))))
  (define-open-bag-method string)
  (define-open-bag-method pathname)
  (define-open-bag-method stream))

;;; bag -> RSB events

;; Relies on string-specialized method.
(defmethod bag->events ((source sequence) (dest t) &rest args &key)
  (unless (length= 1 source)
    (error "~@<~S cannot be applied to ~S: more than one source is not ~
            currently supported.~@:>"
           'bag->events source))
  (apply #'bag->events (first-elt source) dest args))

(defun %bag->events/streamish (source dest
                               &rest args
                               &key
                               backend
                               transform
                               (bag-class 'synchronized-bag)
                               &allow-other-keys)
  (let ((bag (apply #'open-bag source
                    :bag-class bag-class
                    :direction :input
                    (append (when backend
                              (list :backend backend))
                            (when transform
                              (list :transform transform))))))
    (apply #'bag->events bag dest
           (remove-from-plist args :backend :transform :bag-class))))

(macrolet ((define-open-bag-method (type)
             `(defmethod bag->events ((source ,type)
                                      (dest   t)
                                      &rest args &key)
                (apply #'%bag->events/streamish source dest args))))
  (define-open-bag-method string)
  (define-open-bag-method pathname)
  (define-open-bag-method stream))

(defmethod bag->events ((source channel)
                        (dest   string)
                        &rest args &key)
  (apply #'bag->events source (puri:parse-uri dest) args))

(defmethod bag->events ((source channel)
                        (dest   function)
                        &key)
  (make-instance 'endpoint-channel-connection
                 :bag      (channel-bag source)
                 :channels (list source)
                 :endpoint dest))
