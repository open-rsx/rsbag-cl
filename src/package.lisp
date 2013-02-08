;;; package.lisp --- Package definition for the cl-rsbag system.
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

(cl:defpackage :rsbag
  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :more-conditions)

  ;; Symbols
  (:export
   :&from-source)

  ;; Types
  (:export
   :direction

   :if-does-not-exist-policy
   :if-exists-policy

   :transform-spec/default
   :transform-spec/augment
   :transform-spec/full
   :transform-spec)

  ;; Conditions
  (:export
   :rsbag-error

   :open-error
   :open-error-source

   :bag-error
   :bag-error-bag

   :no-such-channel
   :no-such-channel-name

   :read-only-bag

   :channel-error
   :channel-error-channel

   :channel-open-error

   :channel-exists

   :no-such-entry
   :no-such-entry-key)

  ;; meta-data protocol
  (:export
   :meta-data
   :meta-data-count
   :meta-data-keys
   :meta-data-values
   :meta-data-plist
   :meta-data-alist)

  ;; `bag' class and file protocol
  (:export
   :bag

   :bag-location
   :bag-direction
   :bag-transform
   :bag-channels
   :bag-channel

   :open-bag)

  ;; `synchronized-channel' class
  (:export
   :synchronized-channel)

  ;; `synchronized-bag' class
  (:export
   :synchronized-bag)

  ;; `channel' class and channel protocol
  (:export
   :channel

   :channel-bag
   :channel-name
   :channel-meta-data
   :channel-transform

   :%channel-id
   :%channel-backend

   :channel-timestamps
   :channel-entries
   :channel-items

   :entry)

  ;; Time range protocol
  (:export
   :start
   :end)

  ;; Convenience macros
  (:export
   :with-bag)

  ;; Package management macros
  (:export
   :make-versioned-name
   :with-renamed-package
   :with-renamed-packages
   :with-versioned-packages)

  ;; Threadpool
  (:export
   :start-threadpool
   :stop-threadpool
   :enable-restart-threadpool

   :with-threadpool)

  (:documentation
   "This package contains the Common Lisp implementation of RSBag.

The client interface primarily consists of the `file' and `channel'
classes. Conceptually, files consists of multiple named channels which
in turned contain sequences of timestamped data items."))
