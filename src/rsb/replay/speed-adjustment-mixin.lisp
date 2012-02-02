;;; speed-adjustment-mixin.lisp --- Mixin that scales scheduled playback times.
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

(defclass speed-adjustment-mixin ()
  ((speed :initarg  :speed
	  :type     positive-real
	  :accessor strategy-speed
	  :initform 1
	  :documentation
	  "Stores the speed factor that should be applied to the
results of scheduling events."))
  (:documentation
   "This mixin class adds to timed replay strategy classes the ability
to speed up or slow down replay speed by a constant factor."))

(defmethod schedule-event :around ((strategy speed-adjustment-mixin)
				   (event    t)
				   (previous local-time:timestamp)
				   (next     local-time:timestamp))
  (/ (call-next-method) (strategy-speed strategy)))
