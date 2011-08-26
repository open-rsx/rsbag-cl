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
			(wire-schema (required-argument :wire-schema))
			(timestamp   :create)
			&allow-other-keys)
  (bind (((:accessors-r/o (scope participant-scope)
			  (id    participant-id)) source)
	 (name    (scope-string scope))
	 (channel (setf (bag-channel dest name
				     :if-exists :error
				     :transform (make-transform :rsb-event
								(make-keyword wire-schema)))
			(list :source-name   (princ-to-string id)
			      :source-config (princ-to-string
					      (abstract-uri source))
			      :format        "TODO"))))
    (push #'(lambda (event)
	      (setf (entry channel (timestamp event timestamp)) event))
	  (rsb.ep:handlers source))
    (make-instance 'bag-connection
		   :bag      dest
		   :channels (list (make-instance
				    'channel-connection
				    :participant source
				    :channel     channel)))))

(defmethod events->bag ((source puri:uri)
			(dest   bag)
			&rest args &key)
  (bind ((options (rsb:uri-options source))
	 ((:plist wire-schema) options)
	 ((:plist transports) args)
	 (listener (make-listener
		    (puri:merge-uris
		     (format nil "?~{~(~A~)=~A~^;~}"
			     (remove-from-plist options :wire-schema))
		     source)
		    :transports '((:spread :converter :fundamental-null)))))
    (apply #'events->bag listener
	   dest
	   :wire-schema wire-schema
	   (remove-from-plist args :transports))))

(defmethod events->bag ((source string)
			(dest   bag)
			&rest args &key)
  (apply #'events->bag (puri:parse-uri source) dest args))

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


;;; bag -> RSB events
;;

(macrolet ((define-open-bag-method (type)
	     `(defmethod bag->events ((source ,type)
				      (dest   t)
				      &rest args
				      &key
				      backend
				      (bag-class 'synchronized-bag))
		(let ((bag (apply #'open-bag source
				  :bag-class bag-class
				  :direction :input
				  (append (when backend
					    (list :backend backend)))) ))
		  (apply #'bag->events bag dest
			 (remove-from-plist args :backend :bag-class))))))
  (define-open-bag-method string)
  (define-open-bag-method pathname))

(defmethod bag->events ((source bag)
			(dest   puri:uri)
			&rest args
			&key
			(strategy    :recorded-timing)
			(start-index 0)
			end-index
			channels)
  (bind ((predicate (if (eq channels t) (constantly t) channels))
	 (channels  (remove-if-not predicate (bag-channels source)))
	 ((:flet do-channel (channel))
	  (apply #'bag->events channel dest
		 (remove-from-plist args :strategy)))
	 (connections (map 'list #'do-channel channels))
	 (strategy    (make-instance
		       (find-replay-strategy-class strategy)
		       :start-index start-index
		       :end-index   end-index)))
    (make-instance 'replay-bag-connection
		   :bag      source
		   :channels connections
		   :strategy strategy)))

(defmethod bag->events ((source bag)
			(dest   string)
			&rest args &key)
  (apply #'bag->events source (puri:parse-uri dest) args))

(defmethod bag->events ((source channel)
			(dest   puri:uri)
			&key)
  (bind ((name (if (starts-with #\/ (channel-name source))
		   (subseq (channel-name source) 1)
		   (channel-name source)))
	 (uri  (puri:merge-uris name dest))
	 ((:plist type) (channel-meta-data source))
	 (converter   (make-instance
		       'rsb.converter:force-wire-schema
		       :wire-schema (if (listp type)
					(second type)
					:bytes)))
	 (participant (make-informer
		       uri t
		       :transports `((:spread :converter ,converter
					      &inherit)))))
    (make-instance 'channel-connection
		   :channel     source
		   :participant participant)))
