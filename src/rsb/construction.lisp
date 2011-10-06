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
			(timestamp        :create)
			(channel-strategy :scope-and-type)
			&allow-other-keys)
  (make-instance
   'recording-channel-connection
   :bag         dest
   :channels    nil
   :participant source
   :strategy    (make-channel-strategy channel-strategy)))

(defmethod events->bag ((source puri:uri)
			(dest   bag)
			&rest args
			&key
			(transports '((:spread :expose-wire-schema? t
				               &inherit)
				      (:socket :expose-wire-schema? t
				               &inherit)))
			(filters    nil filters-supplied?))
  (let ((listener (make-listener source
				 :transports transports
				 :converters '((t . :fundamental-null)))))
    (when filters-supplied?
      (setf (receiver-filters listener) filters))
    (apply #'events->bag
	   listener
	   dest
	   (remove-from-plist args :transports))))

(defmethod events->bag ((source string)
			(dest   bag)
			&rest args &key)
  (apply #'events->bag (puri:parse-uri source) dest args))

(defmethod events->bag ((source sequence)
			(dest   bag)
			&rest args &key)
  (make-instance 'bag-connection
		 :bag      dest
		 :channels (map 'list
				#'(lambda (source)
				    (apply #'events->bag source dest args))
				source)))

(macrolet ((define-open-bag-method (type)
	     `(defmethod events->bag ((source t)
				      (dest   ,type)
				      &rest args
				      &key
				      backend
				      (bag-class 'synchronized-bag))
		(apply #'events->bag source
		       (apply #'open-bag dest
			      :bag-class bag-class
			      :direction :io
			      (append (when backend
					(list :backend backend))))
		       (remove-from-plist args :backend :bag-class)))))
  (define-open-bag-method string)
  (define-open-bag-method pathname)
  (define-open-bag-method stream))


;;; bag -> RSB events
;;

(macrolet ((define-open-bag-method (type)
	     `(defmethod bag->events ((source ,type)
				      (dest   t)
				      &rest args
				      &key
				      backend
				      (bag-class 'synchronized-bag))
		(apply #'bag->events
		       (apply #'open-bag source
			      :bag-class bag-class
			      :direction :input
			      (append (when backend
					(list :backend backend))))
		       dest
		       (remove-from-plist args :backend :bag-class)))))
  (define-open-bag-method string)
  (define-open-bag-method pathname)
  (define-open-bag-method stream))

(defmethod bag->events ((source bag)
			(dest   puri:uri)
			&rest args
			&key
			(replay-strategy :recorded-timing)
			(start-index     0)
			end-index
			(channels    t))
  (bind ((predicate (if (eq channels t) (constantly t) channels))
	 (channels  (remove-if-not predicate (bag-channels source)))
	 ((:flet do-channel (channel))
	  (apply #'bag->events channel dest
		 (remove-from-plist args :replay-strategy)))
	 (connections (map 'list #'do-channel channels))
	 ((class &rest args) (ensure-list replay-strategy))
	 (strategy (apply #'make-replay-strategy class
			  :start-index start-index
			  :end-index   end-index
			  args)))
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
	 (name (let ((colon-index (position #\: name)))
		 (if colon-index
		     (subseq name 0 colon-index)
		     name)))
	 (uri  (%make-playback-uri name dest))
	 ((:plist type) (channel-meta-data source))
	 (converter   (make-instance
		       'rsb.converter:force-wire-schema
		       :wire-schema (if (listp type)
					(second type)
					:bytes)))
	 (participant (make-informer
		       uri t :converters `((t . ,converter)))))
    (make-instance 'channel-connection
		   :bag         (channel-bag source)
		   :channels    (list source)
		   :participant participant)))


;;; Utility functions
;;

(defun %make-playback-uri (channel-name base-uri)
  "Return a URI that is the result of merging CHANNEL-NAME and
BASE-URI. Normalize the path component of BASE-URI and preserve query
component of BASE-URI, if present."
  (let ((base-uri (puri:copy-uri base-uri)))
    ;; If BASE-URI has a path, ensure it ends with "/" to prevent
    ;; `puri:merge-uri' from acting differently depending on whether
    ;; there is a "/" or not.
    (unless (ends-with #\/ (puri:uri-path base-uri))
      (setf (puri:uri-path base-uri)
	    (concatenate 'string (puri:uri-path base-uri) "/")))
    (let ((result (puri:merge-uris channel-name base-uri)))
      ;; Merging stomps on the query part, if any, of
      ;; BASE-URI. Restore it afterward.
      (when (puri:uri-query base-uri)
	(setf (puri:uri-query result) (puri:uri-query base-uri)))
      result)))
