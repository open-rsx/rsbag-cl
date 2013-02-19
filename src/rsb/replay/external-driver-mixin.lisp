;;; external-driver-mixin.lisp --- Mixin class for externally controlled replay.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
			  (length    (missing-required-argument :length))
			  (step      (missing-required-argument :step))
			  (index     (missing-required-argument :index))
			  (element   (missing-required-argument :element))
			  (emit      (missing-required-argument :emit))
			  (terminate (missing-required-argument :terminate)))
  "Return a default alist of commands."
  `(;; Queries
    (:length         . ,(lambda ()
			  (funcall length)))
    (:relativelength . ,(lambda ()
			  (funcall length t)))
    (:index          . ,(lambda ()
			  (funcall index)))
    (:relativeindex  . ,(lambda ()
			  (funcall index t)))
    (:time           . ,(lambda ()
			  (princ-to-string (first (funcall element)))))
    ;; Position
    (:next           . ,(lambda ()
			  (funcall step nil)
			  (funcall index)))
    (:previous       . ,(lambda ()
			  (funcall step t)
			  (funcall index)))
    (:seek           . ,(lambda (position)
			  (let ((diff (- position (funcall index))))
			    (iter (repeat (abs diff))
				  (funcall step (minusp diff))))
			  (values)))
    ;; Emission
    (:emit           . ,(lambda ()
			  (funcall emit)
			  (values)))
    (:emitandnext    . ,(lambda ()
			  (funcall emit)
			  (funcall step nil)
			  (funcall index)))

    (:get            . ,(lambda ()
			  (event-data (second (funcall element)))))

    ;; Session
    (:quit           . ,(lambda ()
			  (funcall terminate)
			  (values)))))

(defmethod execute-command ((strategy external-driver-mixin)
			    (command  function))
  (funcall command))

(defmethod replay ((connection replay-bag-connection)
		   (strategy   external-driver-mixin)
		   &key
		   progress)
  (let+ (((&accessors-r/o (start-index strategy-start-index)
			  (end-index   strategy-end-index)) strategy)
	 (sequence        (make-view connection strategy))
	 (update-progress (%make-progress-reporter sequence progress))
	 terminate?
	 ;; Iteration state.
	 ((&values current limit from-end?)
	  (sequence:make-simple-sequence-iterator
	   sequence :start start-index :end end-index))
	 (previous-timestamp)
	 ;; Primitive state query and manipulation functions.
         ((&labels length* (&optional relative-to-bounds?)
	    (if relative-to-bounds?
		(- (or end-index (length*)) start-index)
		(length sequence))))
	 ((&flet end? (back?)
	   (sequence:iterator-endp
            sequence current
	    (if back? (1- start-index) limit) (xor back? from-end?))))
	 ((&flet step* (back?)
	    (setf current (sequence:iterator-step
			   sequence current (xor back? from-end?)))
	    (when (end? back?)
	      (setf current (sequence:iterator-step
			     sequence current (xor (not back?) from-end?)))
	      (error "~@<Attempt to step beyond ~:[end~;beginning~] of ~
sequence. Current position ~:D, valid range [~:D, ~:D[.~@:>"
		     back? (sequence:iterator-index sequence current)
		     start-index end-index))))
	 ((&labels index (&optional relative-to-bounds?)
	    (if relative-to-bounds?
		(- (index) start-index)
		(sequence:iterator-index sequence current))))
	 ((&flet element ()
	    (sequence:iterator-element sequence current)))
	 ((&flet emit ()
	    (let+ (((timestamp event informer) (element)))
	      (process-event connection strategy
			     timestamp previous-timestamp
			     event informer)
	      (setf previous-timestamp timestamp))))
	 ((&flet terminate ()
	    (setf terminate? t))))

    (setf (%strategy-commands strategy)
	  (make-commands strategy sequence
			 :length    #'length*
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
