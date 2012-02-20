;;; types.lisp --- Types used in the cl-rsbag system.
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

(cl:in-package :rsbag)

(deftype direction ()
  "Values of this type are used to indicate whether a bag should be
opened for reading, writing or both."
  '(member :input :output :io))


;;; Transformation specifications
;;

(deftype transform-spec/default ()
  "This transform specification causes the default transformation to
be applied."
  'null)

(deftype transform-spec/augment ()
  "This transform specification causes supplied arguments to be
appended when the default transformation is constructed."
  '(cons (eql :from-source) list))

(deftype transform-spec/full ()
  "This transform specification cases the a specific transform to be
constructed with supplied arguments without automatic derivation."
  '(cons symbol list))

(deftype transform-spec ()
  "All forms of transform specifications."
  '(or transform-spec/default
       transform-spec/augment
       transform-spec/full))
