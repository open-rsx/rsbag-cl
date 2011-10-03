;;; package.lisp --- Package definition for the backend.tidelog module.
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

(cl:defpackage :rsbag.backend.tidelog
  (:use
   :cl
   :alexandria
   :split-sequence
   :bind
   :iterate

   :rsbag
   :rsbag.backend)

  ;; Conditions
  (:export
   :tidelog-condition

   :log-file-error
   :log-file-error-source

   :invalid-file-structure)

  (:documentation
   "This package contains a cl-rsbag backend for the TIDE log file
format as specified at https://retf.info/svn/drafts/rd-0001.txt."))
