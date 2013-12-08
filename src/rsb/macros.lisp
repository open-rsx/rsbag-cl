;;;; macros.lisp --- Macros provided by the rsb module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

(defmacro with-open-connection ((var connection-form) &body body)
  "Execute BODY with VAR bound to the connection object that is the
   result of evaluating CONNECTION-FORM. Ensure that the connection is
   properly closed."
  `(with-open-stream (,var ,connection-form)
     ,@body))

(defmacro with-events->bag ((var source dest
                             &rest args
                             &key
                             error-policy
                             transports
                             filters
                             timestamp
                             backend
                             bag-class
                             channel-strategy
                             &allow-other-keys)
                            &body body)
  "Execute BODY with VAR bound to a connection that is the result of
   applying `events->bag' to SOURCE, DEST and ARGS. Ensure that the
   resulting connection is properly closed."
  (declare (ignore error-policy
                   transports filters timestamp backend bag-class
                   channel-strategy))
  `(with-open-connection (,var (events->bag ,source, dest ,@args))
     ,@body))

(defmacro with-bag->events ((var source dest
                             &rest args
                             &key
                             error-policy
                             backend
                             bag-class
                             replay-strategy
                             start-time
                             start-index
                             end-time
                             end-index
                             channels
                             &allow-other-keys)
                            &body body)
  "Execute BODY with VAR bound to a connection that is the result of
   applying `bag->events' to SOURCE, DEST and ARGS. Ensure that the
   resulting connection is properly closed."
  (declare (ignore error-policy
                   backend bag-class replay-strategy
                   start-time start-index end-time end-index
                   channels))
  `(with-open-connection (,var (bag->events ,source ,dest ,@args))
     ,@body))
