;;; remote-controlled.lisp --- Strategy for RPC-controlled replay.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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


;;; Command queue protocol
;;

(defgeneric enqueue (strategy command)
  (:documentation
   "Queue COMMAND for execution by STRATEGY."))


;;; `remote-controlled' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :remote-controlled)))
  (find-class 'remote-controlled))

(defclass remote-controlled (error-policy-mixin
			     external-driver-mixin
			     timestamp-adjustment-mixin
			     uri-mixin)
  ((rsb::uri :accessor strategy-control-uri)
   (server   :accessor %strategy-server
	     :documentation
	     "Stores the server that exposes the replay control
methods to clients.")
   (queue    :type     sb-concurrency:mailbox
	     :reader   %strategy-queue
	     :initform (sb-concurrency:make-mailbox
			:name "Commands")
	     :documentation
	     "Stores a queue of replay control commands."))
  (:default-initargs
   :uri (missing-required-initarg 'remote-controlled :uri))
  (:documentation
   "This strategy exposes replay control commands via an RPC
server. Clients invoke the methods to control the replay. At least the
following commands are available:

+ length      : void   -> uint64
  Return the length of the replayed sequence.
+ index       : void   -> uint64
  Return the current position in the replayed sequence.
+ next        : void   -> uint64
  Move the replay cursor to the next entry, return new index.
+ previous    : void   -> uint64
  Move the replay cursor to the previous entry, return new index.
+ seek        : uint64 -> void
  Position the replay cursor at the supplied entry index.
+ emit        : void   -> void
  Publish the entry at which the replay cursor is currently positioned.
+ emitandnext : void   -> uint64
  Publish the entry at which the replay cursor is currently positioned, advance to the next entry, return new index.
+ get         : void   -> bytes
  Return the entry at which the replay cursor is currently positioned. Do not emit or change anything.
+ quit        : void   -> void
  Terminate the replay."))

(defmethod (setf %strategy-commands) :after ((new-value list)
					     (strategy  remote-controlled))
  "Create methods in the RPC server for the elements of NEW-VALUE."
  (let+ (((&accessors-r/o (server %strategy-server)) strategy)
	 ((&flet make-command (function request future)
	    #'(lambda ()
		(handler-case
		    (let ((result (multiple-value-list
				   (apply function request))))
		      (setf (future-result future)
			    (if result
				(first result)
				rsb.converter:+no-value+))) ;;; TODO(jmoringe): ugly
		  (error (condition)
		    (setf (future-error future) condition)))))))
    ;; Remove registered methods from the server.
    (iter (for method in (server-methods server))
	  (setf (server-method server (method-name method)) nil))

    ;; Create wrapper functions for commands and register
    ;; corresponding server methods.
    (iter (for name-and-lambda in new-value)
	  ;; We cannot use iterate for destructuring since the closed
	  ;; over variables NAME and LAMBDA would change due during
	  ;; iteration.
	  (let+ (((name . lambda) name-and-lambda)
		 (name (format nil "~(~A~)" name)))
	    (setf (server-method server name)
		  #'(lambda (&rest request)
		      (let ((future (make-instance 'rsb.patterns:future)))
			(enqueue strategy (make-command lambda request future))
			(let ((result (future-result future))) ;;; TODO(jmoringe): cumbersome
			  (if (eq result rsb.converter:+no-value+)
			      (values)
			      result)))))))))

(defmethod enqueue ((strategy remote-controlled)
		    (command  t))
  (sb-concurrency:send-message (%strategy-queue strategy) command))

(defmethod next-command ((strategy remote-controlled))
  (sb-concurrency:receive-message (%strategy-queue strategy)))

(defmethod replay ((connection replay-bag-connection)
		   (strategy   remote-controlled)
		   &key &allow-other-keys)
  (let+ (((&accessors (uri    strategy-control-uri)
		      (server %strategy-server)) strategy))
    (with-local-server (server* uri)
      (setf server server*)
      (call-next-method))))
