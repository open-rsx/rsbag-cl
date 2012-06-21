;;; package.lisp --- Package definition for backend module.
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

(cl:defpackage :rsbag.backend
  (:use
   :cl
   :alexandria
   :let-plus
   :more-conditions

   :rsbag)

  ;; Conditions
  (:export
   :log-file-error
   :log-file-error-source

   :invalid-file-structure)

  ;; backend protocol
  (:export
   :backend-location
   :backend-direction

   :get-channels
   :make-channel-id
   :put-channel

   :get-num-entries
   :get-timestamps

   :get-entry
   :put-entry)

  ;; Backend findable class family
  (:export
   :no-such-backend-class
   :find-backend-class
   :backend-classes)

  ;; `stream-mixin' class
  (:export
   :stream-mixin

   :backend-stream)

  ;; `direction-mixin' class
  (:export
   :direction-mixin)

  ;; `location-mixin' class
  (:export
   :location-mixin)

  ;; `buffering-writer-mixin' class and protocol
  (:export
   :buffering-writer-mixin

   :buffer-property

   :backend-buffer
   :make-buffer
   :write-buffer
   :flush

   :backend-flush-strategy)

  ;; `last-write-time-mixin'
  (:export
   :last-write-time-mixin)

  ;; Flush strategy protocol
  (:export
   :flush?)

  ;; Flush strategy class family
  (:export
   :no-such-flush-strategy-class
   :find-flush-strategy-class
   :flush-strategy-classes

   :make-flush-strategy)

  (:documentation
   "This package contains protocol and implementation aids for file
format backends for cl-rsbag."))
