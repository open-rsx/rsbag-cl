;;; package.lisp --- Package definition for the rsb.replay module.
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

(cl:defpackage :rsbag.rsb.replay
  (:shadowing-import-from :rsbag
   :direction

   :meta-data
   :meta-data-count
   :meta-data-keys
   :meta-data-values
   :meta-data-plist
   :meta-data-alist)

  (:use
   :cl
   :alexandria
   :split-sequence
   :bind
   :iterate

   :rsbag
   :rsbag.transform
   :rsbag.view
   :rsbag.rsb

   :rsb
   :rsb.patterns)

  ;; `error-policy-mixin' mixin class
  (:export
   :error-policy-mixin)

  ;; bounds protocol and mixin class
  (:export
   :strategy-start-index
   :strategy-end-index

   :bounds-mixin)

  ;; view creation protocol and mixin class
  (:export
   :make-view

   :view-creation-mixin)

  ;; sequential processing protocol and mixin class
  (:export
   :process-event

   :sequential-mixin)

  ;; speed adjustment protocol and mixin class
  (:export
   :strategy-speed

   :speed-adjustment-mixin)

  ;; external driver protocol and mixin class
  (:export
   :make-commands
   :strategy-commands
   :find-command
   :next-command
   :execute-command

   :external-driver-mixin)

  ;; `recorded-timing' replay strategy class
  (:export
   :recorded-timing)

  ;; `fixed-rate' replay strategy class
  (:export
   :fixed-rate

   :strategy-rate
   :strategy-delay)

  ;; `as-fast-as-possible' replay strategy class
  (:export
   :as-fast-as-possible)

  ;; `remote-controlled' replay strategy class
  (:export
   :remote-controlled)

  ;; `interactive' replay strategy class
  (:export
   :interactive

   :strategy-stream
   :strategy-previous-command)

  (:documentation
   "This package contains supporting infrastructure and replay
strategy classes."))
