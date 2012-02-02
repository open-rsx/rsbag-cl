;;; protocol.lisp --- Protocol for the view module.
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

(cl:in-package :rsbag.view)


;;; View construction functions
;;

(defgeneric make-serialized-view (sequences
				  &key
				  selector)
  (:documentation
   "Make and return a sequence that consists of a serialization of the
elements of SEQUENCES. The serialization is performed by comparing
timestamps of elements arranging elements in the order of increasing
timestamps.
SELECTOR is a function that is applied to each element of SEQUENCES
before the view is constructed. When SEQUENCES is a sequence of
`channel's or a `bag', functions such as `channel-timestamps',
`channel-entries' and `channel-items' can be supplied as SELECTOR to
select the elements of the returned sequence."))


;;; Extensible support functions
;;

(defgeneric %make-key-function (sequence)
  (:documentation
   "Return a function that takes four arguments, a sequence, an
iterator, a limit and a from-end-value, and returns a serialization
key (for example a `local-time:timestamp') for the corresponding
element. The returned function has to be applicable to sequences of
the type of SEQUENCE and associated iterators."))
