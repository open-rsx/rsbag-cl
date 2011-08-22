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
   :direction)

  (:use
   :cl
   :alexandria
   :bind
   :iterate

   :rsbag

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

  ;; `channel-connection' class and protocol
  (:export
   :channel-connection

   :connection-channel
   :connection-participant)

  (:documentation
   "This package contains functions and classes that enable the
recording and playback of RSB events into/from rsbag log files."))
