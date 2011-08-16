;;; package.lisp --- Package definition for backend module.
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

(cl:defpackage :rsbag.backend
  (:use
   :cl
   :alexandria
   :bind)

  ;; backend protocol
  (:export
   :get-channels
   :make-channel-id
   :put-channel

   :get-num-entries
   :get-timestamps

   :get-entry
   :put-entry)

  ;; `stream-mixin' class
  (:export
   :stream-mixin

   :backend-stream)

  ;; `buffering-writer-mixin' class and protocol
  (:export
   :buffering-writer-mixin

   :backend-buffer
   :make-buffer
   :write-buffer)

  (:documentation
   "This package contains protocol and implementation aids for file
format backends for cl-rsbag."))
