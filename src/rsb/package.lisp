;;; package.lisp --- Package definition for rsb module.
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

(cl:defpackage :rsbag.rsb
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

   :rsb)

  ;; connection construction protocol
  (:export
   :events->bag
   :bag->events)

  ;; connection protocol
  (:export
   :done?
   :wait)

  ;; `bag-connection' class and protocol
  (:export
   :bag-connection

   :connection-bag
   :connection-channels)

  ;; `replay-bag-connection' subclass and protocol
  (:export
   :replay-bag-connection

   :connection-strategy)

  ;; `channel-connection' class and protocol
  (:export
   :channel-connection

   :connection-participant)

  ;; channel allocation strategy protocol
  (:export
   :channel-name-for
   :make-channel-for

   :no-such-channel-strategy-class
   :find-channel-strategy-class
   :channel-strategy-classes

   :make-channel-strategy)

  ;; replay strategy protocol
  (:export
   :replay

   :schedule-event

   :no-such-replay-strategy-class
   :find-replay-strategy-class
   :replay-strategy-classes

   :make-replay-strategy)

  ;; `fixed-rate' replay strategy class
  (:export
   :fixed-rate

   :strategy-rate
   :strategy-delay)

  (:documentation
   "This package contains functions and classes that enable the
recording and playback of RSB events into/from rsbag log files."))
