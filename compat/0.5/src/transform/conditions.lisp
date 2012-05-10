;;; conditions.lisp --- Conditions used in the transform module.
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

(cl:in-package :rsbag.transform)

(define-condition transform-error (error)
  ((transform :initarg  :transform
	      :reader   transform-error-transform
	      :documentation
	      "Stores the transform instance that was used in the
failed transform operation."))
  (:report
   (lambda (condition stream)
     (format stream "~@<A transformation involving the transform ~A ~
failed.~@:>"
	     (transform-error-transform condition))))
  (:documentation
   "Errors of this condition class and its subclasses are signaled
when a transform fails."))

(define-condition encoding-error (transform-error
				  chainable-condition)
  ((domain-object :initarg  :domain-object
		  :reader   transform-error-domain-object
		  :documentation
		  "Stores the domain object the encoding of which
failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The domain object ~S could not be encoded by ~
the transform ~A.~/more-conditions::maybe-print-cause/~@:>"
	     (transform-error-domain-object condition)
	     (transform-error-transform     condition)
	     condition)))
  (:documentation
   "This error is signaled when the encoding of a domain object for
storage in bag fails."))

(define-condition decoding-error (transform-error
				  chainable-condition)
  ((encoded :initarg  :encoded
	    :reader   transform-error-encoded
	    :documentation
	    "Stores the encoded data, the decoding of which failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The encoded value ~S could not be decoded by ~
the transform ~A.~/more-conditions::maybe-print-cause/~@:>"
	     (transform-error-encoded   condition)
	     (transform-error-transform condition)
	     condition)))
  (:documentation
   "This error is signaled when the decoding of data, usually
retrieved from a bag, fails."))
