;;; conditions.lisp --- Conditions used in backend modules.
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

(cl:in-package :rsbag.backend)

(define-condition log-file-error (rsbag-error)
  ((source :initarg  :source
	   :reader   log-file-error-source
	   :documentation
	   "Stores the source involved in the error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<An error has been encountered when operating ~
on ~A.~@:>"
	     (log-file-error-source condition))))
  (:documentation
   "Errors of this class and subclasses are signaled when operations
involving log files fail."))

(define-condition invalid-file-structure (simple-error
					  log-file-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Invalid file structure encountered in ~
~A~/more-conditions::maybe-print-explanation/~@:>"
	     (log-file-error-source condition)
	     condition)))
  (:documentation
   "This error is signaled if an invalid file structure is encountered
while reading a log file."))
