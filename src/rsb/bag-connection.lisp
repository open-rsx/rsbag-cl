;;;; bag-connection.lisp --- A class for bag <-> RSB connections.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; `bag-connection' class

(defclass bag-connection (rsb.ep:error-policy-mixin)
  ((bag      :initarg  :bag
             :reader   connection-bag
             :documentation
             "Stores the bag object that is connected to RSB
              participants as a data source or sink.")
   (channels :initarg  :channels
             :type     list
             :reader   connection-channels
             :initform nil
             :documentation
             "Stores a list of channel connections for channels of the
              bag that are connected to RSB participants as data
              sources or sinks."))
  (:default-initargs
   :bag (missing-required-initarg 'bag-connection :bag))
  (:documentation
   "Instances of this class represent the connections being
    established when channels of bags are used as data sources or
    sinks of RSB participants. "))

(defmethod shared-initialize :after ((instance   bag-connection)
                                     (slot-names t)
                                     &key)
  (setf (rsb.ep:processor-error-policy instance)
        (rsb.ep:processor-error-policy instance)))

(defmethod (setf rsb.ep:processor-error-policy) :before ((new-value t)
                                                         (object    bag-connection))
  (iter (for channel in (connection-channels object))
        (setf (rsb.ep:processor-error-policy channel) new-value)))

(defmethod close ((connection bag-connection)
                  &key &allow-other-keys)
  "Close all channel connections, then close the bag."
  (map nil #'close (connection-channels connection))
  (close (connection-bag connection)))

(defmethod wait ((connection bag-connection))
  "Wait for all channel connections."
  (map nil #'wait (connection-channels connection)))

(defmethod start ((connection bag-connection))
  (map nil #'start (connection-channels connection)))

(defmethod stop ((connection bag-connection))
  (map nil #'stop (connection-channels connection)))

(defmethod print-object ((object bag-connection) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (connection-channels object)))))

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
                                      (connection-channels connection))))
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

;;; `replay-bag-connection' class

(defclass replay-bag-connection (bag-connection)
  ((strategy :initarg  :strategy
             :reader    connection-strategy
             :documentation
             "Stores the strategy that is used for replaying events
              from the associated bag of the connection."))
  (:default-initargs
   :strategy (missing-required-initarg 'replay-bag-connection :strategy))
  (:documentation
   "Instances of this class associated an event replay strategy, a
    source bag and `rsb:informer' instances to collaboratively replay
    the events from the bag."))

(defmethod (setf rsb.ep:processor-error-policy) :before ((new-value t)
                                                         (object    replay-bag-connection))
  (let+ (((&accessors-r/o (strategy connection-strategy)) object))
    (setf (rsb.ep:processor-error-policy strategy) new-value)))
