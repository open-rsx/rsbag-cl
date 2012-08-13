;;; flush-strategies.lisp --- Flush strategy classes provided by backend module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsbag.backend)


;;; `property-limit' strategy class
;;

(defmethod find-flush-strategy-class ((spec (eql :property-limit)))
  (find-class 'property-limit))

(defclass property-limit ()
  ((property :initarg  :property
	     :type     keyword
	     :accessor flush-strategy-property
	     :documentation
	     "Stores the name of the buffer property based on whose
value the flushing decision should be made.")
   (limit    :initarg  :limit
	     :type     real
	     :accessor flush-strategy-limit
	     :documentation
	     "Stores the "))
  (:default-initargs
   :property (missing-required-initarg 'property-limit :property)
   :limit    (missing-required-initarg 'property-limit :limit))
  (:documentation
   "This strategy causes a buffer to be flushed every time a specified
property violates a given limit."))

(defmethod flush? ((strategy property-limit)
		   (backend  t)
		   (buffer   t))
  (let+ (((&accessors-r/o (property flush-strategy-property)
			  (limit    flush-strategy-limit)) strategy))
    (> (buffer-property backend buffer property) limit)))

(defmethod print-object ((object property-limit) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S @ ~:D"
	    (flush-strategy-property object)
	    (flush-strategy-limit    object))))


;;; `composite-flush-strategy-mixin' mixin class
;;

(defclass composite-flush-strategy-mixin ()
  ((children :initarg  :children
	     :type     list
	     :accessor children
	     :initform nil
	     :documentation
	     "A list of child strategies which are consulted to
produce a decision."))
  (:documentation
   "This class is intended to be mixed into flush strategy classes
which produce their decisions by consulting subordinate strategies."))

(defmethod print-object ((object composite-flush-strategy-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A (~D)"
	    (class-name (class-of object))
	    (length (children object)))))

(macrolet
    ((define-simple-composite-strategy (name
					&key
					(spec       (make-keyword name))
					(class-name (format-symbol "FLUSH-IF-~A" name))
					reducer)
       `(progn
	  (defmethod find-flush-strategy-class ((spec (eql ,spec)))
	    (find-class ',class-name))

	  (defclass ,class-name (composite-flush-strategy-mixin)
	    ()
	    (:documentation
	     ,(format nil "This strategy flushes buffers when ~A of ~
its child strategies indicate that buffers should be flushed."
		      reducer)))

	  (defmethod flush? ((strategy ,class-name)
			     (backend  t)
			     (buffer   t))
	    (,reducer (rcurry #'flush? backend buffer) (children strategy)))

	  (defmethod make-flush-strategy ((thing (eql (find-class ',class-name)))
					  &rest args)
	    (make-instance
	     thing
	     :children (mapcar (curry #'apply #'make-flush-strategy)
			       args))))))

  (define-simple-composite-strategy or
    :class-name flush-if-some
    :reducer    some)
  (define-simple-composite-strategy and
    :class-name flush-if-every
    :reducer    every)
  (define-simple-composite-strategy not
    :class-name flush-if-notany
    :reducer    notany))