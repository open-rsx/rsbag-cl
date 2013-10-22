;;;; synchronized-channel.lisp --- A channel that synchronizes accesses.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defclass synchronized-channel (channel)
  ((lock :initarg  :lock
         :accessor %channel-lock
         :documentation
         "The lock that is used to synchronize accesses to the
          channel. Usually points to a lock owned by the containing
          bag."))
  (:default-initargs
   :lock (required-argument :lock))
  (:documentation
   "Instances of this channel class can be safely used from multiple
    threads. Callers have to be prepared to encounter increased
    latencies in comparison to the single-threaded case."))

(macrolet
    ((define-synchronized-method (name args)
       `(defmethod ,name :around ,args
                   (bt:with-lock-held ((%channel-lock channel))
                     (call-next-method)))))
  (define-synchronized-method
      channel-timestamps ((channel synchronized-channel)))
  (define-synchronized-method
      entry ((channel synchronized-channel)
             (index   t)
             &key &allow-other-keys))
  (define-synchronized-method
      (setf entry) ((new-value t)
                    (channel   synchronized-channel)
                    (index     t)
                    &key &allow-other-keys)))
