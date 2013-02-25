;;; package.lisp --- Package definition for the rsb.replay module.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
   :let-plus
   :iterate
   :more-conditions

   :rsbag
   :rsbag.transform
   :rsbag.view
   :rsbag.rsb

   :rsb
   :rsb.patterns)

  ;; Types
  (:export
   :range-boundary/timestamp)

  ;; `error-policy-mixin' mixin class
  (:export
   :error-policy-mixin)

  ;; index bounds protocol and mixin class
  (:export
   :strategy-start-index
   :strategy-end-index

   :bounds-mixin)

  ;; time bounds protocol and mixin class
  (:export
   :strategy-start-time
   :strategy-end-time

   :time-bounds-mixin)

  ;; view creation protocol and mixin class
  (:export
   :make-view

   :view-creation-mixin)

  ;; sequential processing protocol and mixin class
  (:export
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

  ;; `delay-correcting-mixin' mixin class
  (:export
   :delay-correcting-mixin

   :strategy-previous-delay
   :strategy-previous-call)

  ;; `timestamp-adjustment-mixin' mixin class
  (:export
   :timestamp-adjustment-mixin

   :strategy-adjustments)

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
