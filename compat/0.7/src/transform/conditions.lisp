;;; conditions.lisp --- Conditions used in the transform module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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
     (let ((*print-length* (or *print-length* 16)))
       (format stream "~@<The domain object ~S could not be encoded by ~
the transform ~A.~/more-conditions::maybe-print-cause/~@:>"
	       (transform-error-domain-object condition)
	       (transform-error-transform     condition)
	       condition))))
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
     (let ((*print-length* (or *print-length* 16)))
       (format stream "~@<The encoded value ~S could not be decoded by ~
the transform ~A.~/more-conditions::maybe-print-cause/~@:>"
	       (transform-error-encoded   condition)
	       (transform-error-transform condition)
	       condition))))
  (:documentation
   "This error is signaled when the decoding of data, usually
retrieved from a bag, fails."))
