;;;; bag-connections.lisp --- Classes for replay bag connections.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; `replay-connection-mixin' class

(defclass replay-connection-mixin ()
  ((strategy :initarg  :strategy
             :reader    connection-strategy
             :documentation
             "Stores the strategy that is used for replaying events
              from the associated bag."))
  (:default-initargs
   :strategy (missing-required-initarg 'replay-connection-mixin :strategy))
  (:documentation
   "Stores a strategy for replaying events from a source."))

(defmethod (setf rsb.ep:processor-error-policy) :before
    ((new-value t)
     (object    replay-connection-mixin))
  (let+ (((&structure-r/o connection- strategy) object))
    (setf (rsb.ep:processor-error-policy strategy) new-value)))

;;; `replay-bag-connection' class

(defclass replay-bag-connection (bag-connection
                                 replay-connection-mixin)
  ()
  (:documentation
   "Associates a replay strategy, a source bag and sinks to
    collaboratively replay events."))
