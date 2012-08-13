;;; conditions.lisp --- Conditions used in the TIDE log backend of cl-rsbag.
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

(cl:in-package :rsbag.backend.tidelog)

(define-condition tidelog-condition (condition)
  ()
  (:documentation
   "This condition class serves as a superclass for TIDELOG-related
condition classes."))

(define-condition tidelog-file-error (log-file-error
				      tidelog-condition)
  ()
  (:documentation
   "Errors of this class and subclasses are signaled when operations
involving TIDE log file fail."))

(define-condition invalid-tidelog-structure (invalid-file-structure
					     tidelog-condition)
  ()
  (:documentation
   "This error is signaled if an invalid file structure is encountered
is encountered while reading a TIDE log file."))