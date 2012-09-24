;;; package.lisp --- Package definition for rsb module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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
   :let-plus
   :iterate
   :more-conditions

   :rsbag
   :rsbag.transform
   :rsbag.view

   :rsb)

  ;; Conditions
  (:export
   :connection-error
   :connection-error-connection

   :recording-error

   :event-storage-failed
   :connection-error-event

   :replay-error
   :connection-error-strategy

   :event-retrieval-failed)

  ;; connection construction protocol
  (:export
   :events->bag
   :bag->events)

  ;; connection protocol
  (:export
   :done?
   :wait

   :start
   :stop)

  ;; Convenience macros
  (:export
   :with-open-connection
   :with-events->bag
   :with-bag->events)

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

   :connection-endpoint)

  ;; `participant-channel-connection' subclass
  (:export
   :participant-channel-connection)

  ;; channel allocation strategy protocol
  (:export
   :channel-name-for
   :channel-format-for
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

  (:documentation
   "This package contains functions and classes that enable the
recording and playback of RSB events into/from rsbag log files."))
