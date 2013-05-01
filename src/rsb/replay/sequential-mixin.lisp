;;;; sequential-mixin.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

(defclass sequential-mixin (replay-restart-mixin
                            time-bounds-mixin
                            view-creation-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that essentially process all events in a sequential manner. The method
on `replay' for `sequential-mixin' creates a sequence via `make-view'
and processes all elements of the sequence by sequential calls to
`process-event'."))

(defmethod replay ((connection replay-bag-connection)
                   (strategy   sequential-mixin)
                   &key
                   progress)
  (let+ (((&accessors-r/o (start-index strategy-start-index)
                          (end-index   strategy-end-index)) strategy)
         (sequence        (make-view connection strategy))
         (update-progress (%make-progress-reporter sequence progress)))
    (macrolet
        ((do-it (&optional end-index)
           `(iter (for (timestamp event sink) each sequence
                       :from start-index
                       ,@(when end-index '(:below end-index)))
                  (for previous-timestamp previous timestamp)
                  (for i :from start-index)
                  (process-event connection strategy
                                 timestamp previous-timestamp
                                 event sink)
                  (when update-progress
                    (funcall update-progress i timestamp)))))
      (if end-index
          (do-it end-index)
          (do-it)))))

(defmethod process-event ((connection         replay-bag-connection)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              (eql :skip))
                          (sink               t))
  "Error recovery behaviors may inject the value :skip for EVENT. The
default behavior is just ignoring the failed event. "
  (values))

(defmethod process-event ((connection         replay-bag-connection)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              t)
                          (sink               t))
  "The default behavior consists in sending EVENT via SINK which is
assumed to be an `rsb:informer'."
  (send sink event :unchecked? t))

(defmethod process-event ((connection         replay-bag-connection)
                          (strategy           sequential-mixin)
                          (timestamp          t)
                          (previous-timestamp t)
                          (event              t)
                          (sink               function))
  "The default behavior for a function SINK consists in calling SINK
with EVENT."
  (funcall sink event))
