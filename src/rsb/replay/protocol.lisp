;;;; protocol.lisp --- Protocol functions of the rsb.replay module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

;;; Bounds protocol

(defgeneric strategy-start-index (strategy)
  (:documentation
   "Return the start index of the region processed by STRATEGY."))

(defgeneric strategy-end-index (strategy)
  (:documentation
   "Return the end index of the region processed by STRATEGY."))

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
STRATEGY to NEW-VALUE. When NEW-VALUE is nil STRATEGY does not impose
such a constraint."))

;;; External driver protocol

(defgeneric make-commands (strategy sequence
                           &key
                           length step index element emit terminate)
  (:documentation
   "Return an alist of items of the form (NAME . FUNCTION) that should
be used as the command list of STRATEGY and SEQUENCE. The values of
LENGTH, STEP, INDEX, ELEMENT, EMIT and TERMINATE are functions that
perform the respective action for SEQUENCE."))

(defgeneric strategy-commands (strategy)
  (:documentation
   "Return an alist of items of the form (NAME . FUNCTION) consisting
of the available commands for STRATEGY."))

(defgeneric find-command (strategy name
                          &key
                          error?)
  (:documentation
   "Find and return the command named NAME within the list of
available commands for STRATEGY. If ERROR? is non-nil (the default),
signal an error if NAME does not designate a command."))

(defgeneric next-command (strategy)
  (:documentation
   "Determine and return the next command that should be executed for
STRATEGY. The returned command should be a thunk, usually from the
list of available commands of STRATEGY."))

(defgeneric execute-command (strategy command)
  (:documentation
   "Execute the thunk COMMAND in a an appropriate way for STRATEGY."))
