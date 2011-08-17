;;; package.lisp --- Package definition for the cl-rsbag system.
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

(cl:defpackage :rsbag
  (:use
   :cl
   :alexandria
   :iterate
   :bind

   :rsbag.backend)

  ;; `bag' class and file protocol
  (:export
   :bag

   :bag-channels
   :bag-channel

   :open-bag)

  ;; `channel' class and channel protocol
  (:export
   :channel

   :channel-id
   :channel-name

   :entry)

  ;; Convenience macros
  (:export
   :with-bag)

  (:documentation
   "This package contains the Common Lisp implementation of RSBag.

The client interface primarily consists of the `file' and `channel'
classes. Conceptually, files consists of multiple named channels which
in turned contain sequences of timestamped data items."))