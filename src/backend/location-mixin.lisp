;;; location-mixin.lisp --- Location information for backend objects.
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

(defclass location-mixin ()
  ((location :initarg  :location
	     :accessor backend-location
	     :initform nil
	     :documentation
	     "Stores the location to which the backend object is
connected. Can be NIL is such a location is not known."))
  (:documentation
   "This mixin allows remembering the location to which
a (e.g. stream-based) backend object is connected."))
