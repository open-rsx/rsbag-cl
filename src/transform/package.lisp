;;; package.lisp --- Package definition for the transform module.
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

(cl:defpackage :rsbag.transform
  (:use
   :cl
   :alexandria
   :bind
   :iterate)

  (:import-from :rsb
   :chainable-condition
   :chainable-condition-cause)

  ;; Variables
  (:export
   :+rsb-schema-name+)

  ;; Conditions
  (:export
   :transform-error
   :transform-error-transform

   :encoding-error
   :transform-error-domain-object

   :decoding-error
   :transform-error-encoded)

  ;; Transform protocol
  (:export
   :transform-name

   :decode
   :encode)

  ;; Findable transform class family
  (:export
   :no-such-transform-class
   :find-transform-class
   :transform-classes

   :make-transform)

  (:documentation
   "This package contains the transformation protocol and
infrastructure used in cl-rsbag."))
