;;; macros.lisp --- Convenience macros provided by the cl-rsbag system.
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

(in-package :rsbag)

(defmacro with-bag ((var source
		     &rest args
		     &key &allow-other-keys)
		    &body body)
  "Execute BODY with VAR bound to a bag object for the data source
SOURCE. ARGS are passed to `open-bag'."
  (check-type var symbol "a symbol")

  `(let ((,var (open-bag ,source ,@args)))
     (unwind-protect
	  (progn ,@body)
       (close ,var))))
