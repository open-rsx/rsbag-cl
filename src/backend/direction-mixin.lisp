;;; direction-mixin.lisp --- Mixin class for direction-sensitive backends
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

(in-package :rsbag.backend)

(defclass direction-mixin ()
  ((direction :initarg  :direction
	      :type     rsbag:direction
	      :reader   backend-direction
	      :documentation
	      "Stores the direction with which the backend has been
opened."))
  (:default-initargs
   :direction (required-argument :direction))
  (:documentation
   "This class is intended to be mixed into backend classes that have
to keep track of the direction for which the data source has been
opened."))
