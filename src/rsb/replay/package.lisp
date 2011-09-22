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
   :bind
   :iterate

   :rsbag
   :rsbag.transform
   :rsbag.view
   :rsbag.rsb

   :rsb)

  ;; view creation protocol
  (:export
   :make-view)

  ;; sequential processing protocol
  (:export
   :process-event)

  ;; `fixed-rate' replay strategy class
  (:export
   :fixed-rate

   :strategy-rate
   :strategy-delay)

  ;; `as-fast-as-possible' replay strategy class
  (:export
   :as-fast-as-possible)

  (:documentation
   "TODO"))
