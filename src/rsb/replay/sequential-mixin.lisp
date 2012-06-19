;;; sequential-mixin.lisp ---
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

(defclass sequential-mixin (replay-restart-mixin
			    time-bounds-mixin
			    view-creation-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that essentially process all events in a sequential manner. The method
on `replay' for `sequential-mixin' creates a sequence via `make-view'
and processes all elements of the sequence by sequential calls to
`process-event'."))

(defmethod replay ((connection replay-bag-connection)
		   (strategy   sequential-mixin)
		   &key
		   progress)
  (let+ (((&accessors-r/o (start-index strategy-start-index)
			  (end-index   strategy-end-index)) strategy)
	 (sequence        (make-view connection strategy))
	 (update-progress (%make-progress-reporter sequence progress)))
    (macrolet
	((do-it (&optional end-index)
	   `(iter (for (timestamp event informer) each     sequence
		       :from start-index
		       ,@(when end-index '(:to end-index)))
		  (for previous-timestamp         previous timestamp)
		  (for i :from start-index)
		  (process-event connection strategy
				 timestamp previous-timestamp
				 event informer)
		  (when update-progress
		    (funcall update-progress i timestamp)))))
      (if end-index
	  (do-it end-index)
	  (do-it)))))

(defmethod process-event :around ((connection         replay-bag-connection)
				  (strategy           sequential-mixin)
				  (timestamp          t)
				  (previous-timestamp t)
				  (event              t)
				  (informer           t))
  "Install a continue restart around processing."
  (restart-case
      (call-next-method)
    (continue ()
      :report (lambda (stream)
		(format stream "~@<Ignore the failed event and ~
continue with the next event.~@:>")
		nil)
      (values))))

(defmethod process-event ((connection         replay-bag-connection)
			  (strategy           sequential-mixin)
			  (timestamp          t)
			  (previous-timestamp t)
			  (event              (eql :skip))
			  (informer           t))
  "Error recovery behaviors may inject the value :skip for EVENT. The
default behavior is just ignoring the failed event. "
  (values))

(defmethod process-event ((connection         replay-bag-connection)
			  (strategy           sequential-mixin)
			  (timestamp          t)
			  (previous-timestamp t)
			  (event              t)
			  (informer           t))
  "The default behavior consists in sending EVENT via INFORMER."
  (send informer event :unchecked? t))

(defmethod process-event ((connection         replay-bag-connection)
			  (strategy           sequential-mixin)
			  (timestamp          t)
			  (previous-timestamp t)
			  (event              t)
			  (sink               function))
  "The default behavior for a function SINK consists in calling SINK
with EVENT."
  (funcall sink event))
