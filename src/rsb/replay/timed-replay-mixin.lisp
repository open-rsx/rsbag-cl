;;; timed-replay-mixin.lisp ---
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

(defclass timed-replay-mixin (sequential-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy
classes perform time-based scheduling of replayed events."))

(defmethod process-event :before ((connection         replay-bag-connection)
				  (strategy           timed-replay-mixin)
				  (timestamp          local-time:timestamp)
				  (previous-timestamp local-time:timestamp)
				  (event              t)
				  (informer           t))
  "Delay the publishing of EVENT for the amount of time computed by
`schedule-event'."
  (let ((amount (schedule-event strategy event previous-timestamp timestamp)))
    (when (plusp amount)
      (sleep amount))))
