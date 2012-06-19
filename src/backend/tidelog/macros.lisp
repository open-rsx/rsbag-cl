;;; macros.lisp --- Macros for code generation.
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

(cl:in-package :rsbag.backend.tidelog)

(defmacro define-element ((name) &body specs-and-options)
  (check-type name symbol "a symbol")

  (let+ (((&values specs (&plist-r/o (documentation :documentation)))
	  (parse-specs-and-options specs-and-options)))
    `(progn
       ,(specs->class name specs :documentation documentation)
       ,(specs->size name specs)
       ,(specs->serializer name specs)
       ,(specs->deserializer name specs))))

(defun parse-specs-and-options (specs-and-options)
  (let ((specs   (remove-if     #'keywordp specs-and-options
				:key #'first))
	(options (remove-if-not #'keywordp specs-and-options
				:key #'first)))
    (values specs (apply #'append options))))
