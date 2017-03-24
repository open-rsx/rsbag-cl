;;;; protocol.lisp --- Unit tests for the protocol of the rsb module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.test)

(deftestsuite events->bag-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `events->bag' function."))

(addtest (events->bag-root
          :documentation
          "Test the `events->bag' function with a participant
           source.")
  participant-source

  (ensure-cases (args events expected)
      `(;; Invalid channel strategy => error
        ((:channel-strategy :no-such-strategy)
         nil
         service-provider:missing-provider-error)

        ;; These are valid.
        ((:channel-strategy :scope-and-type)
         (1 2 3)
         (,(octet-vector 1 0 0 0)
          ,(octet-vector 2 0 0 0)
          ,(octet-vector 3 0 0 0)))
        ((:introspection-survey? t)
         (1 2 3)
         (,(octet-vector 1 0 0 0)
          ,(octet-vector 2 0 0 0)
          ,(octet-vector 3 0 0 0)))
        ((:introspection-survey? nil)
         (1 2 3)
         (,(octet-vector 1 0 0 0)
          ,(octet-vector 2 0 0 0)
          ,(octet-vector 3 0 0 0))))

    (let+ (((&flet do-it ()
              (rsbag.test:with-mock-bag (bag :direction :output) ()
                (with-open-connection
                    (connection (apply #'events->bag '("inprocess:") bag args))
                  (rsb:with-participant (informer :informer "inprocess:")
                    (mapc (lambda (datum)
                            (let ((buffer (make-octet-vector 4)))
                              (setf (ub32ref/le buffer 0) datum)
                              (rsb:send informer buffer
                                        :rsb.transport.wire-schema :uint32)))
                          events))
                  (map 'list #'rsb:event-data (first (bag-channels bag))))))))
     (case expected
       (service-provider:missing-provider-error
        (ensure-condition 'service-provider:missing-provider-error (do-it)))
       (t
        (ensure-same (do-it) expected :test #'equalp))))))

(addtest (events->bag-root
          :documentation
          "Test the `events->bag' function without a source.")
  no-source

  (ensure-cases (args timestamps-and-events expected)
      `(;; Invalid channel strategy => error
        ((:channel-strategy :no-such-strategy)
         ()
         service-provider:missing-provider-error)

        ;; These are valid.
        (()
         ((,(local-time:now)
           ("/foo" #1=,(octet-vector 1 2 3 4) "int32")))
         (#1#))
        ((:timestamp :receive)
         ((,(local-time:now)
           ("/foo" #2=,(octet-vector 1 2 3 4) "int32")))
         (#2#))
        ((:channel-stratetgy :scope-and-type)
         ((,(local-time:now)
           ("/foo" #3=,(octet-vector 1 2 3 4) "int32")))
         (#3#)))

    (let+ (((&flet make-event (scope payload wire-schema)
              (let ((event (rsb:make-event
                            scope payload
                            :rsb.transport.wire-schema wire-schema)))
                (setf (rsb:event-origin event)          (uuid:make-null-uuid)
                      (rsb:event-sequence-number event) 0)
                event)))
           ((&flet do-it ()
              (rsbag.test:with-mock-bag (bag :direction :output) ()
                (with-open-connection
                    (connection (apply #'events->bag nil bag args))
                  (mapc (lambda+ ((timestamp (scope payload wire-schema)))
                          (let ((event (make-event scope payload wire-schema)))
                            (rsbag.rsb.recording:process-event
                             connection timestamp event)))
                        timestamps-and-events))
                (map 'list #'rsb:event-data (first (bag-channels bag)))))))
      (case expected
        (service-provider:missing-provider-error
         (ensure-condition 'service-provider:missing-provider-error (do-it)))
        (t
         (ensure-same (do-it) expected :test #'equalp))))))

(deftestsuite bag->events-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `bag->events' function."))

(addtest (bag->events-root
          :documentation
          "Smoke test for the `bag->events' function.")
  smoke

  (ensure-cases (source args &optional expected)
      `(;; Invalid channel strategy => error
        (,(rsbag.test:simple-bag)    (:replay-strategy :no-such-strategy)
         service-provider:missing-provider-error)

        ;; Cannot supply and arguments which would have applied to
        ;; opening the bag => error
        (,(rsbag.test:simple-bag)    (:backend   :does-not-matter)
         incompatible-arguments)
        (,(rsbag.test:simple-bag)    (:transform :does-not-matter)
         incompatible-arguments)
        (,(rsbag.test:simple-bag)    (:bag-class :does-not-matter)
         incompatible-arguments)

        ;; These are valid.
        (,(rsbag.test:simple-bag)    (:replay-strategy :as-fast-as-possible))
        ((,(rsbag.test:simple-bag))  (:replay-strategy :as-fast-as-possible))
        (#(,(rsbag.test:simple-bag)) (:replay-strategy :as-fast-as-possible)))

    (let+ (((&flet do-it ()
              (collecting-events (record)
                (with-open-connection
                    (connection
                     (apply #'bag->events source
                            (lambda (timestamp event)
                              (declare (ignore timestamp))
                              (record event))
                            args))
                  (rsbag.rsb.replay:replay connection (connection-strategy connection)))
                (record)))))
      (case expected
        (service-provider:missing-provider-error
         (ensure-condition 'service-provider:missing-provider-error (do-it)))
        (incompatible-arguments
         (ensure-condition 'incompatible-arguments (do-it)))
        (t
         (ensure-same
          (length (do-it))
          (reduce #'+ (bag-channels (rsbag.test:simple-bag))
                  :key #'length)))))))

(addtest (bag->events-root
          :documentation
          "Ensure that events replayed via RSB by `bag->events' get
           the configured prefix scope.")
  prefix-scope

  (ensure-cases (prefix)
      '("/" "/prefix")

    (rsb:with-participant (reader :reader "inprocess:")
      ;; Send the events stored in the mock bag.
      (with-open-connection
          (connection (bag->events
                       (rsbag.test:simple-bag) (format nil "inprocess:~A" prefix)
                       :replay-strategy :as-fast-as-possible))
        (rsbag.rsb.replay:replay connection (connection-strategy connection)))
      ;; Receive the events.
      (iter (repeat (reduce #'+ (bag-channels (rsbag.test:simple-bag))
                            :key #'length))
            (let ((scope (rsb:event-scope (rsb:receive reader))))
              (ensure (not (rsb:scope= scope prefix)))
              (ensure (rsb:sub-scope? scope prefix)))))))
