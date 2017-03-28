;;;; bag-connection.lisp --- A class for bag <-> RSB connections.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; `composite-connection-mixin' class

(defclass composite-connection-mixin ()
  ((connections :initarg :connections
                :type     list #|of connections|#
                :reader   connection-direct-connections
                :initform '()
                :documentation
                "Stores a list of child connections."))
  (:documentation
   "Intended to be mixed into connection classes with child connections."))

(defmethod shared-initialize :after ((instance   composite-connection-mixin)
                                     (slot-names t)
                                     &key)
  (setf (rsb.ep:processor-error-policy instance)
        (rsb.ep:processor-error-policy instance)))

(defmethod (setf rsb.ep:processor-error-policy) :before
    ((new-value t)
     (object    composite-connection-mixin))
  (iter (for connection in (connection-direct-connections object))
        (setf (rsb.ep:processor-error-policy connection) new-value)))

(defmethod close ((connection composite-connection-mixin) &key abort)
  ;; Close all direct child connections.
  (map nil (rcurry #'close :abort abort)
       (connection-direct-connections connection)))

(defmethod wait ((connection composite-connection-mixin))
  (map nil #'wait (connection-direct-connections connection)))

(defmethod start ((connection composite-connection-mixin))
  (map nil #'start (connection-direct-connections connection)))

(defmethod stop ((connection composite-connection-mixin))
  (map nil #'stop (connection-direct-connections connection)))

(defmethod print-items:print-items append ((object composite-connection-mixin))
  `((:direct-connection-count ,(length (connection-direct-connections object))
                              "(~D)")))

;;; `bag-connection' class

(defclass bag-connection (composite-connection-mixin
                          rsb.ep:error-policy-mixin
                          print-items:print-items-mixin)
  ((bag :initarg  :bag
        :reader   connection-bag
        :documentation
        "Stores the bag object that is connected to the data source(s)
         or sink(s)."))
  (:default-initargs
   :bag (missing-required-initarg 'bag-connection :bag))
  (:documentation
   "Connections between channels of bags and data sources or sinks."))

(defmethod close ((connection bag-connection) &key abort)
  (call-next-method)
  (close (connection-bag connection) :abort abort))

;;; `recording-bag-connection' class

(defclass recording-bag-connection (bag-connection)
  ((introspection-survey? :initarg  :introspection-survey?
                          :reader   connection-introspection-survey?
                          :initform t
                          :documentation
                          "Controls whether the connection should
                           perform an RSB introspection survey when
                           the recording is started."))
  (:documentation
   "Instances of this class are used to record events into a bag."))

(defmethod start :after ((connection recording-bag-connection))
  ;; After the recording has been started, perform an introspection
  ;; survey, recording the introspection replies. This inserts an
  ;; introspection snapshot at the beginning of the recording in
  ;; relation to which the differential introspection information in
  ;; the remainder of the recording can be interpreted.
  (when (connection-introspection-survey? connection)
    (log:info "~@<~A is performing introspection survey.~@:>" connection)
    (restart-case
        ;; TODO the transport should normalize this
        (let+ (((&flet normalize-url (url)
                  (puri:copy-uri url :path nil :parsed-path nil :fragment nil)))
               (urls (mapcar #'normalize-url
                             (mappend (compose #'rsb:transport-specific-urls
                                               #'connection-endpoint)
                                      (connection-connections
                                       connection :include-inner? nil))))
               (urls (remove-duplicates urls :test #'puri:uri=)))
          (rsb:with-participant
              (introspection :remote-introspection
                             rsb.introspection:+introspection-scope+
                             :receiver-uris   urls
                             :update-interval nil
                             :error-policy    #'continue)
            (declare (ignore introspection))))
      (continue ()
        :report (lambda (stream)
                  (format stream "~@<Continue recording using ~A ~
                                  without an introspection ~
                                  survey.~@:>"
                          connection))))))
