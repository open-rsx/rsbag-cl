;;;; view-creation-mixin.lisp --- Mixin class for creating multi-channel views.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.rsb.replay)

(defclass view-creation-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that have to construct a view sequence for multiple channels. The
generic function `make-view' can be used to customize this
behavior. The method for `view-creation-mixin' creates a
serialized view of events across channels."))

(defmethod make-view ((connection replay-bag-connection)
                      (strategy   view-creation-mixin)
                      &key
                      (selector (rcurry #'inject-informer connection)))
  "Default behavior is serializing events across channels."
  (make-serialized-view
   (mappend #'connection-channels (connection-channels connection))
   :selector selector))
