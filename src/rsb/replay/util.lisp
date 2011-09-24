;;; util.lisp ---
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsbag.rsb.replay)


;;; `informer-injector' helper class
;;

(defclass informer-injector (channel-items)
  ((informer :initarg  :informer
	     :reader   %informer-injector-informer
	     :documentation
	     "Stores the informer that should be associated with the
channel."))
  (:documentation
   "Instance of this helper class inject a given object (usually an
`rsb:informer' instance) into each element of the underlying
sequence."))

(defmethod sequence:elt ((sequence informer-injector)
			 (index    integer))
  (append (call-next-method)
	  (list (%informer-injector-informer sequence))))

(defun inject-informer (channel connection)
  ;; Find the channel-connection for CHANNEL in CONNECTION, extract
  ;; the informer and pass it to a new `informer-injector' instance.
  (make-instance 'informer-injector
		 :channel  channel
		 :informer (connection-participant
			    (find channel (connection-channels connection)
				  :test #'member
				  :key  #'connection-channels))))


;;; Utility functions
;;

(defun %make-progress-reporter (sequence callback)
  "Return a function with two parameters that calls CALLBACK in the
appropriate way if CALLBACK is non-nil"
  (if callback
      (bind (((start end) (list 0 (length sequence))))
	#'(lambda (index timestamp)
	    (funcall callback
		     (/ (1+ (- index start)) (- end start))
		     index start end
		     timestamp)))
      #'(lambda (index timestamp)
	  (declare (ignore index timestamp)))))
