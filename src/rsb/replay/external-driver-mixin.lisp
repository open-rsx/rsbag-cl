;;; external-driver-mixin.lisp --- Mixin class for externally controlled replay.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsbag.rsb.replay)


;;; External driver protocol
;;

(defgeneric make-commands (strategy sequence
			   &key
			   step index element emit terminate)
  (:documentation
   "Return an alist of items of the form (NAME . FUNCTION) that should
be used as the command list of STRATEGY and SEQUENCE. The values of
STEP, INDEX, ELEMENT, EMIT and TERMINATE are functions that perform
the respective action for SEQUENCE."))

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


;;; `external-driver-mixin' mixin class
;;

(defclass external-driver-mixin (sequential-mixin)
  ((commands :type     list
	     :reader   strategy-commands
	     :accessor %strategy-commands
	     :initform nil
	     :documentation
	     "Stores available commands as an alist of items of the
form (NAME . IMPLEMENTING-FUNCTION)."))
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that depend on some kind of external driver to control iteration
through the sequential data. Instances store a list of available
commands which are available for invocation by the external driver
mechanism. The `replay' method basically retrieves subsequent commands
and executes them until termination is requested."))

(defmethod find-command ((strategy external-driver-mixin)
			 (name     string)
			 &key
			 (test   #'string=)
			 (error? t))
  (or (cdr (assoc name (strategy-commands strategy)
		  :test test
		  :key  #'symbol-name))
      (when error?
	(error "~@<No such command: ~S.~@:>" name))))

(defmethod make-commands ((strategy external-driver-mixin)
			  (sequence sequence)
			  &key
			  step
			  index
			  element
			  emit
			  terminate)
  "Return a default alist of commands."
  `(;; Queries
    (:length      . ,#'(lambda ()
			 (length sequence)))
    (:index       . ,#'(lambda ()
			 (funcall index)))
    (:time        . ,#'(lambda ()
			 (princ-to-string (first (funcall element)))))
    ;; Position
    (:next        . ,#'(lambda ()
			 (funcall step nil)
			 (funcall index)))
    (:previous    . ,#'(lambda ()
			 (funcall step t)
			 (funcall index)))
    (:seek        . ,#'(lambda (position)
			 (let ((diff (- position (funcall index))))
			   (iter (repeat (abs diff))
				 (funcall step (minusp diff))))
			 (values)))
    ;; Emission
    (:emit        . ,#'(lambda ()
			 (funcall emit)
			 (values)))
    (:emitandnext . ,#'(lambda ()
			 (funcall emit)
			 (funcall step nil)
			 (funcall index)))
    ;; Session
    (:quit        . ,#'(lambda ()
			 (funcall terminate)
			 (values)))))

(defmethod execute-command ((strategy external-driver-mixin)
			    (command  function))
  (funcall command))

(defmethod replay ((connection replay-bag-connection)
		   (strategy   external-driver-mixin)
		   &key
		   progress)
  (bind (((:accessors-r/o (start-index strategy-start-index)
			  (end-index   strategy-end-index)) strategy)
	 (sequence        (make-view connection strategy))
	 (update-progress (%make-progress-reporter sequence progress))
	 terminate?
	 ;; Iteration state
	 ((:values current _ from-end?)
	  (sequence:make-simple-sequence-iterator
	   sequence :start start-index :end end-index))
	 (previous-timestamp)
	 ;; Primitive state manipulation functions
	 ((:flet step* (back?))
	  (setf current (sequence:iterator-step
			 sequence current (xor back? from-end?))))
	 ((:flet index ())
	  (sequence:iterator-index sequence current))
	 ((:flet element ())
	  (sequence:iterator-element sequence current))

	 ((:flet emit ())
	  (bind (((timestamp event informer) (element)))
	    (process-event connection strategy
			   timestamp previous-timestamp
			   event informer)
	    (setf previous-timestamp timestamp)))
	 ((:flet terminate ())
	  (setf terminate? t)))

    (setf (%strategy-commands strategy)
	  (make-commands strategy sequence
			 :step      #'step*
			 :index     #'index
			 :element   #'element
			 :emit      #'emit
			 :terminate #'terminate))

    (iter (until terminate?)
	  (for command next (next-command strategy))
	  (execute-command strategy command)
	  (when update-progress
	    (funcall update-progress (index) (first (element)))))))
