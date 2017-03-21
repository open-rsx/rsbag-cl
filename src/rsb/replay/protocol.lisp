;;;; protocol.lisp --- Protocol functions of the rsb.replay module.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; Replay protocol

(defgeneric replay (connection strategy &key progress)
  (:documentation
   "Replay the events contained in the associated bag of CONNECTION
    according to STRATEGY. Usually, STRATEGY will mostly influence the
    timing of the replay. However, things like simulated loss of
    events or transformations are also possible.

    If PROGRESS is non-nil it has to be a function accepting five
    arguments: progress ratio, current index, start index, end index
    and current timestamp.

    May signal a `replay-error'. In particular,
    `entry-retrieval-error' and `entry-processing-error' may be
    signaled."))

(define-condition-translating-method replay ((connection t) (strategy t)
                                             &key &allow-other-keys)
  (((and error (not replay-error)) entry-retrieval-error)
   :connection connection
   :strategy   strategy))

;;; Sequential processing protocol

(defgeneric process-event (connection strategy
                           timestamp previous-timestamp
                           event sink)
  (:documentation
   "Process the tuple (TIMESTAMP PREVIOUS-TIMESTAMP EVENT SINK),
    originating from CONNECTION, according to STRATEGY."))

(define-condition-translating-method process-event ((connection         t)
                                                    (strategy           t)
                                                    (timestamp          t)
                                                    (previous-timestamp t)
                                                    (event              t)
                                                    (sink               t))
  ((error entry-processing-error)
   :connection connection
   :strategy   strategy
   :entry      event))

;;; Timed replay protocol

(defgeneric schedule-event (strategy event previous next)
  (:documentation
   "Return a relative time in seconds at which EVENT should be
    replayed according to STRATEGY given timestamps PREVIOUS and NEXT
    of the previous and current event when recorded. PREVIOUS can be
    nil at the start of a replay."))

(defmethod schedule-event ((strategy t)
                           (event    t)
                           (previous (eql nil))
                           (next     local-time:timestamp))
  0)

;;; Replay strategy service

(service-provider:define-service strategy
  (:documentation
   "Providers implement event replay strategies.

    The main difference between strategies is the handling of
    timing."))

(defgeneric make-strategy (spec &rest args)
  (:documentation
   "Return (potentially creating it first) an instance of the replay
    strategy designated by SPEC."))

(defmethod make-strategy ((spec standard-object) &rest args)
  (if args
      (apply #'reinitialize-instance spec args)
      spec))

(defmethod make-strategy ((spec symbol) &rest args)
  (if (keywordp spec)
      (apply #'service-provider:make-provider 'strategy spec
             args)
      (let ((provider (find spec (service-provider:service-providers 'strategy)
                            :key  (compose #'class-name
                                           #'service-provider:provider-class)
                            :test #'eq)))
        (apply #'service-provider:make-provider 'strategy provider
               args))))

(defmethod make-strategy ((spec class) &rest args)
  (let ((provider (find spec (service-provider:service-providers 'strategy)
                        :key  #'service-provider:provider-class
                        :test #'eq)))
    (apply #'service-provider:make-provider 'strategy provider
           args)))

(defmethod make-strategy ((spec cons) &rest args)
  (apply #'make-strategy (first spec) (append (rest spec) args)))

;;; Bounds protocol

(defgeneric strategy-start-index (strategy)
  (:documentation
   "Return the start index of the region processed by STRATEGY."))

(defgeneric strategy-end-index (strategy)
  (:documentation
   "Return the end index of the region processed by STRATEGY."))

;;; View creation protocol

(defgeneric make-view (connection strategy &key selector)
  (:documentation
   "Make and return a sequence view of the events associated to
    CONNECTION for replay according to STRATEGY.

    See `rsbag.view:make-serialized-view' for a description of
    SELECTOR."))

;;; Fixed-rate strategy protocol

(defgeneric strategy-delay (strategy)
  (:documentation
   "Return the delay in seconds between events replayed with
    STRATEGY."))

(defgeneric (setf strategy-delay) (new-value strategy)
  (:documentation
   "Set the delay in seconds between events replayed with STRATEGY to
    NEW-VALUE."))

(defgeneric strategy-rate (strategy)
  (:documentation
   "Return the rate in Hertz of events replayed with STRATEGY."))

(defgeneric (setf strategy-rate) (new-value strategy)
  (:documentation
   "Set the rate in Hertz of events replayed with STRATEGY to
    NEW-VALUE."))

;;; Delay limiting protocol

(defgeneric strategy-max-delay (strategy)
  (:documentation
   "Return the maximum delay between adjacent events permitted by
    STRATEGY or nil if STRATEGY does not impose such a constraint."))

(defgeneric (setf strategy-max-delay) (new-value strategy)
  (:documentation
   "Set the maximum delay between adjacent events permitted by
    STRATEGY to NEW-VALUE. When NEW-VALUE is nil STRATEGY does not
    impose such a constraint."))

;;; External driver protocol

(defgeneric make-commands (strategy sequence
                           &key
                           length step index element emit terminate)
  (:documentation
   "Return an alist of items of the form (NAME . FUNCTION) that should
    be used as the command list of STRATEGY and SEQUENCE. The values
    of LENGTH, STEP, INDEX, ELEMENT, EMIT and TERMINATE are functions
    that perform the respective action for SEQUENCE."))

(defgeneric strategy-commands (strategy)
  (:documentation
   "Return an alist of items of the form (NAME . FUNCTION) consisting
    of the available commands for STRATEGY."))

(defgeneric find-command (strategy name
                          &key
                          error?)
  (:documentation
   "Find and return the command named NAME within the list of
    available commands for STRATEGY. If ERROR? is non-nil (the
    default), signal an error if NAME does not designate a command."))

(defgeneric next-command (strategy)
  (:documentation
   "Determine and return the next command that should be executed for
    STRATEGY. The returned command should be a thunk, usually from the
    list of available commands of STRATEGY."))

(defgeneric execute-command (strategy command)
  (:documentation
   "Execute the thunk COMMAND in a an appropriate way for STRATEGY."))
