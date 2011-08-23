;;; protocol.lisp --- Protocol functions of the transform module.
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

(in-package :rsbag.transform)


;;; Transform protocol
;;

(defgeneric transform-name (transform)
  (:documentation
   "Return a keyword identifying TRANSFORM."))

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
