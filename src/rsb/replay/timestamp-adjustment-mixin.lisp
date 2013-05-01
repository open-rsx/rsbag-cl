;;;; timestamp-adjustment-mixin.lisp --- Adjust event timestamp during replay.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

(defclass timestamp-adjustment-mixin ()
  ((adjustments :type     list
                :accessor strategy-adjustments
                :initform nil
                :documentation
                "Stores a list of adjustments of the form

  (TIMESTAMP NEW-VALUE)

where TIMESTAMP is a keyword designating a timestamp and NEW-VALUE
specifies the new value. Currently, NEW-VALUE can be the symbol :NOW
or a `local-time:timestamp' object."))
  (:documentation
   "This mixin class adds the ability to adjust event timestamps
during replay."))

(defmethod shared-initialize :after ((instance   timestamp-adjustment-mixin)
                                     (slot-names t)
                                     &key
                                     (adjustments nil adjustments-supplied?))
  (when adjustments-supplied?
    (setf (strategy-adjustments instance) adjustments)))

(defmethod (setf strategy-adjustments) :before ((new-value t)
                                                (object    timestamp-adjustment-mixin))
  (check-type new-value list "a list of timestamp adjustment specifications")
  (iter (for spec in new-value)
        (check-type spec timestamp-adjustment-spec)))

(defmethod process-event :before ((connection         replay-bag-connection)
                                  (strategy           timestamp-adjustment-mixin)
                                  (timestamp          t)
                                  (previous-timestamp t)
                                  (event              event)
                                  (sink               t))
  "The default behavior consists in sending EVENT via INFORMER."
  (iter (for (key value) in (strategy-adjustments strategy))
        (setf (timestamp event key)
              (etypecase value
                (timestamp-adjustment-value/now
                 (local-time:now))

                (timestamp-adjustment-value/copy
                 (let ((key (second value)))
                   (or (timestamp event key)
                       (error "~@<Event ~A does not have a ~A ~
timestamp.~:@>"
                              event key))))

                (timestamp-adjustment-value/delta
                 (let+ (((&values sec nsec) (floor (second value))))
                   (local-time:adjust-timestamp (timestamp event key)
                     (:offset :sec  sec)
                     (:offset :nsec (floor (* 1000000000 nsec))))))

                (local-time:timestamp
                 value)))))

(defmethod print-object ((object timestamp-adjustment-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[~A~]"
            (mapcar #'first (strategy-adjustments object)))))
