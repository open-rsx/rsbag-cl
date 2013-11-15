;;;; util.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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
                            (find channel (connection-channels connection)
                                  :test #'member
                                  :key  #'connection-channels))))

;;; Utility functions

(defun %make-progress-reporter (sequence callback)
  "Return a function of two parameters that calls CALLBACK in the
   appropriate way if CALLBACK is non-nil."
  (when callback
    (let ((start 0)
          (end   (1- (length sequence))))
      (lambda (index timestamp)
        (funcall callback
                 (/ (1+ (- index start)) (- (1+ end) start))
                 index start end timestamp)))))
