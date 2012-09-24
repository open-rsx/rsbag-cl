;;; protocol.lisp --- Protocol functions of the transform module.
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


;;; Transform protocol
;;

(defgeneric transform-name (transform)
  (:documentation
   "Return an object identifying TRANSFORM."))

(defgeneric transform-format (transform)
  (:documentation
   "Return an object describing the encoding performed by
TRANSFORM."))

(defgeneric encode (transform domain-object)
  (:documentation
   "Encode DOMAIN-OBJECT using TRANSFORM and return the result."))

(defgeneric decode (transform data)
  (:documentation
   "Decode DATA using TRANSFORM and return the decoded
domain-object."))


;;; Default behavior
;;

(defmethod transform-name ((transform standard-object))
  "Default behavior is to use the class name of TRANSFORM to identify
TRANSFORM."
  (nth-value 0 (make-keyword (class-name (class-of transform)))))

(defmethod encode :around ((transform     t)
			   (domain-object t))
  "Establish a use-value restart and wrap arbitrary conditions in an
`encoding-error' instance."
  (with-condition-translation
      (((error encoding-error)
	:transform     transform
	:domain-object domain-object))
    (restart-case
	(call-next-method)
      (use-value (value)
	:report      (lambda (stream)
		       (format stream "~@<Specify a value to use ~
instead of the result of the failed encoding.~@:>"))
	:interactive (lambda ()
		       (format *query-io* "~@<Enter replacement ~
value (unevaluated): ~@:>")
		       (force-output *query-io*)
		       (list (read *query-io*)))
	value))))

(defmethod decode :around ((transform t)
			   (data      t))
  "Establish a use-value restart and wrap arbitrary conditions in a
`decoding-error' instance."
  (with-condition-translation
      (((error decoding-error)
	:transform transform
	:encoded   data))
    (restart-case
	(call-next-method)
      (use-value (value)
	:report      (lambda (stream)
		       (format stream "~@<Specify a value to use ~
instead of the result of the failed decoding.~@:>"))
	:interactive (lambda ()
		       (format *query-io* "~@<Enter replacement ~
value (unevaluated): ~@:>")
		       (force-output *query-io*)
		       (list (read *query-io*)))
	value))))


;;; Findable transform class family
;;

(dynamic-classes:define-findable-class-family transform
    "This class family consists of transformations that are applied to
entries prior to serializing/after deserializing them to/from bag
channels.")

(defgeneric make-transform (spec
			    &rest args)
  (:documentation
   "Make and return an instance of the transform class designated by
SPEC passing ARGS to the constructed instance."))

(defmethod make-transform ((spec symbol)
			   &rest args)
  (apply #'make-instance (find-transform-class spec) args))
