;;;; util.lisp --- Utilities used in the rsb.replay module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; Class `informer-inject'

(defclass informer-injector (channel-items)
  ((informer :initarg  :informer
             :reader   informer-injector-%informer
             :documentation
             "Stores the informer that should be associated with the
              channel."))
  (:default-initargs
   :informer (missing-required-initarg 'informer-injector :informer))
  (:documentation
   "Instance of this helper class inject a given object (usually an
    `rsb:informer' instance) into each element of the underlying
    sequence."))

(defmethod sequence:elt ((sequence informer-injector)
                         (index    integer))
  (append (call-next-method)
          (list (informer-injector-%informer sequence))))

(defun inject-informer (channel connection)
  ;; Find the channel-connection for CHANNEL in CONNECTION, extract
  ;; the informer and pass it to a new `informer-injector' instance.
  (make-instance 'informer-injector
                 :channel  channel
                 :informer (connection-endpoint
                            (find channel (connection-connections
                                           connection :include-inner? nil)
                                  :test #'member
                                  :key  #'connection-channels))))

;;; Utility functions

;;; Return a function of two parameters that calls CALLBACK in the
;;; appropriate way if CALLBACK is non-nil.
(defun %make-progress-reporter (sequence callback)
  (when callback
    (let ((start 0)
          (end   (max 0 (1- (length sequence)))))
      (lambda (index timestamp)
        (if index
            (funcall callback
                     (/ (1+ (- index start)) (- (1+ end) start))
                     index start end timestamp)
            (funcall callback nil nil nil nil nil))))))

;;; Printing

(defun format-source (stream source &optional colon at)
  (declare (ignore colon at))
  (etypecase source
    (bag-connection
     (format stream "bag ~A" (connection-bag source)))))
