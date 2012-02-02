;;; replay-restart-mixin.lisp --- replay-restart-mixin mixin class.
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

(defclass replay-restart-mixin ()
  ()
  (:documentation
   "This mixin class add the establishing of continue and log restarts
around the actual work of the `replay' method."))

(defmethod replay :around ((connection replay-bag-connection)
			   (strategy   replay-restart-mixin)
			   &key &allow-other-keys)
  (handler-bind
      ((error #'(lambda (condition)
		  (restart-case
		      (error 'event-retrieval-failed
			     :connection connection
			     :strategy   strategy
			     :cause      condition)
		    (continue ()
		      :report (lambda (stream)
				(format stream "~@<Ignore the ~
failed event and continue with the next event.~@:>"))
		      (use-value :skip))
		    (log (&optional condition)
		      :report (lambda (stream)
				(format stream "~@<Log an error ~
message and continue with the next event.~@:>"))
		      (log1 :error "Failed to retrieve an event for replay: ~A"
			    condition)
		      (use-value :skip))))))
    (call-next-method)))
