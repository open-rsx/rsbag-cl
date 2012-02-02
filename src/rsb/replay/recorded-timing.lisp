;;; recorded-timing.lisp ---
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


;;; `recorded-timing' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :recorded-timing)))
  (find-class 'recorded-timing))

(defclass recorded-timing (error-policy-mixin
			   timed-replay-mixin
			   delay-correcting-mixin
			   speed-adjustment-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded and,
as much as possible, with identical local temporal relations. A
faithful replay with respect to global temporal relations (e.g. time
between first and last event) is not attempted explicitly."))

(defmethod schedule-event ((strategy recorded-timing)
			   (event    t)
			   (previous local-time:timestamp)
			   (next     local-time:timestamp))
  (local-time:timestamp-difference next previous))
