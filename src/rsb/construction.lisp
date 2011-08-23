;;; construction.lisp --- Construction of channel <-> events connections.
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

(in-package :rsbag.rsb)


;;; RSB events -> bag
;;

(defmethod events->bag ((source listener)
			(dest   bag)
			&key
			(timestamp :create)
			&allow-other-keys)
  (bind (((:accessors-r/o (scope participant-scope)
			  (id    participant-id)) source)
	 (name    (scope-string scope))
	 (channel (setf (bag-channel dest name :if-exists :error)
			(list :type          "TODO"
			      :source-name   (princ-to-string id)
			      :source-config (princ-to-string
					      (abstract-uri source))
			      :format        "TODO"))))
    (push #'(lambda (event)
	      (setf (entry channel (timestamp event timestamp))
		    (event-data event)))
	  (rsb.ep:handlers source))
    (make-instance 'bag-connection
		   :bag      dest
		   :channels (list (make-instance
				    'channel-connection
				    :participant source
				    :channel     channel)))))

(macrolet ((define-make-listener-method (type)
	     `(defmethod events->bag ((source ,type)
				      (dest   bag)
				      &rest args &key)
		(bind (((:plist transports) args)
		       (listener (make-listener source
						:transports '((:spread :converter :fundamental-null)))))
		  (apply #'events->bag listener
			 dest (remove-from-plist args :transports))))))
  (define-make-listener-method string)
  (define-make-listener-method puri:uri))

(defmethod events->bag ((source sequence)
			(dest   bag)
			&rest args &key)
  (let ((connections
	 (map 'list #'(lambda (source)
			(apply #'events->bag source dest args))
	      source)))
    (make-instance
     'bag-connection
     :bag      (connection-bag (first connections))
     :channels (mappend #'connection-channels connections))))

(macrolet ((define-open-bag-method (type)
	     `(defmethod events->bag ((source t)
				      (dest   ,type)
				      &rest args &key)
		(bind (((:plist backend
				(bag-class :bag-class 'synchronized-bag)) args)
		       (bag (apply #'open-bag dest
				   :bag-class bag-class
				   :direction :io
				   (append (when backend
					     (list :backend backend))))))
		  (apply #'events->bag source bag
			 (remove-from-plist args :backend :bag-class))))))
  (define-open-bag-method string)
  (define-open-bag-method pathname))