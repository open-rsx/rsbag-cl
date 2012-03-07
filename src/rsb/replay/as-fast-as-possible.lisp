;;; as-fast-as-possible.lisp ---
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

(cl:in-package :rsbag.rsb.replay)


;;; `as-fast-as-possible' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :as-fast-as-possible)))
  (find-class 'as-fast-as-possible))

(defclass as-fast-as-possible (error-policy-mixin
			       sequential-mixin
			       timestamp-adjustment-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded, but
as fast as possible. Consequently, recorded timestamps are only used
to establish the playback order of events, but not for any kind of
replay timing."))
