;;;; remote-controlled.lisp --- Strategy for RPC-controlled replay.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; Command queue protocol

(defgeneric enqueue (strategy command)
  (:documentation
   "Queue COMMAND for execution by STRATEGY."))

;;; `remote-controlled' replay strategy class

(defclass remote-controlled (error-policy-mixin
                             filtering-mixin
                             external-driver-mixin
                             timestamp-adjustment-mixin
                             rsb:uri-mixin
                             print-items:print-items-mixin)
  ((rsb::uri :accessor strategy-control-uri)
   (server   :accessor strategy-%server
             :documentation
             "Stores the server that exposes the replay control
              methods to clients.")
   (queue    :type     lparallel.queue:queue
             :reader   strategy-%queue
             :initform (lparallel.queue:make-queue)
             :documentation
             "Stores a queue of replay control commands."))
  (:default-initargs
   :uri (missing-required-initarg 'remote-controlled :uri))
  (:documentation
   "Exposes replay control commands via an RPC server.

    Clients invoke the methods to control the replay. At least the
    following commands are available:

    length(): uint64

      Return the length of the sequence of all events.

    relativelength(): uint64

      Return the length of the replayed (sub-)sequence of all events.

    index(): uint64

      Return the current position in the replayed sequence.

    relativeindex(): uint64

      Return the current position relative to the start of the
      replayed (sub-)sequence.

    next(): uint64

      Move the replay cursor to the next entry, return new index.

    previous(): uint64

      Move the replay cursor to the previous entry, return new index.

    seek(new-position: uint64): void

      Position the replay cursor at the supplied entry index.

    emit(): void

      Publish the entry at which the replay cursor is currently
      positioned.

    emitandnext(): uint64

      Publish the entry at which the replay cursor is currently
      positioned, advance to the next entry, return new index.

    get(): bytes

      Return the entry at which the replay cursor is currently
      positioned. Do not emit or change anything.

    quit(): void

      Terminate the replay."))

(service-provider:register-provider/class
 'strategy :remote-controlled :class 'remote-controlled)

(defmethod (setf strategy-%commands) :after ((new-value list)
                                             (strategy  remote-controlled))
  ;; Create methods in the RPC server for the elements of NEW-VALUE.
  (let+ (((&accessors-r/o (server strategy-%server)) strategy)
         ((&flet make-command (function request future)
            (lambda ()
              (handler-case
                  (let ((result (multiple-value-list
                                 (apply function request))))
                    (setf (rsb.patterns.request-reply:future-result future)
                          (if result
                              (first result)
                              rsb.converter:+no-value+))) ; TODO(jmoringe): ugly
                (error (condition)
                  (setf (rsb.patterns.request-reply:future-error future)
                        condition)))))))
    ;; Remove registered methods from the server.
    (iter (for method in (rsb.patterns.request-reply:server-methods server))
          (setf (rsb.patterns.request-reply:server-method
                 server (rsb.patterns.request-reply:method-name method))
                nil))

    ;; Create wrapper functions for commands and register
    ;; corresponding server methods.
    (iter (for name-and-lambda in new-value)
          ;; We cannot use iterate for destructuring since the closed
          ;; over variables NAME and LAMBDA would change due during
          ;; iteration.
          (let+ (((name . lambda) name-and-lambda)
                 (name (string-downcase name)))
            (setf (rsb.patterns.request-reply:server-method server name)
                  (lambda (&rest request)
                    (let ((future (make-instance 'rsb.patterns.request-reply:future)))
                      (enqueue strategy (make-command lambda request future))
                      (let ((result (rsb.patterns.request-reply:future-result future))) ; TODO(jmoringe): cumbersome
                        (if (eq result rsb.converter:+no-value+)
                            (values)
                            result)))))))))

(defmethod enqueue ((strategy remote-controlled)
                    (command  t))
  (lparallel.queue:push-queue command (strategy-%queue strategy)))

(defmethod next-command ((strategy remote-controlled))
  (lparallel.queue:pop-queue (strategy-%queue strategy)))

(defmethod replay ((connection replay-bag-connection)
                   (strategy   remote-controlled)
                   &key &allow-other-keys)
  (let+ (((&structure strategy- control-uri (server %server) num-repetitions)
          strategy))
    (rsb:with-participant (server* :local-server control-uri)
      (setf server server*)
      (call-repeatedly num-repetitions (lambda () (call-next-method))))))
