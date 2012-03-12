;;; types.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
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


;;; Bounds specifications
;;

(deftype range-boundary/timestamp ()
  "Specification of a boundary of a temporal range.
+ NIL
    Use actual beginning or end of the sequence to be bounded.
+ a `non-negative-real'
    Offset in seconds to the start of the sequence.
+ a `negative-real'
    Negative offset in seconds to the end of the sequence.
+ a `local-time:timestamp'
    Absolute point in time within the sequence."
  '(or null
       non-negative-real
       negative-real
       local-time:timestamp))
