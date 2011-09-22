;;; bounds-mixin.lisp ---
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

(in-package :rsbag.rsb.replay)



;;; `bounds-mixin' mixin class
;;

(defclass bounds-mixin ()
  ((start-index :initarg  :start-index
		:type     non-negative-integer
		:accessor strategy-start-index
		:initform 0
		:documentation
		"Stores the index of the event at which the replay
should start.")
   (end-index   :initarg  :end-index
		:type     (or null non-negative-integer)
		:accessor strategy-end-index
		:initform nil
		:documentation
		"Stores the index after the event at which the replay
should stop or nil if the replay should end at the final event of the
recording."))
  (:documentation
   "Provides start-index and end-index slots and a method on
`print-object'."))

(defmethod print-object ((object bounds-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~D, ~:[*~;~D~]["
	    (strategy-start-index object)
	    (strategy-end-index   object))))
