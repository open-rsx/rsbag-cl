;;; elt-via-iterator-mixin.lisp --- Mixin class for non-random-access sequences
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

(in-package :rsbag.view)

(defclass elt-via-iterator-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into sequence classes that
cannot provide `sequence:elt' directly but can provide
iterators. Subclasses inherit a method on `sequence:elt' that
positions an iterator on the requested index and retrieves the element
from it."))

(defmethod sequence:elt ((view  elt-via-iterator-mixin)
			 (index integer))
  ;; Create an iterator and advance it to INDEX.
  (bind (((:values iterator _ from-end)
	  (sequence:make-simple-sequence-iterator view)))
    (iter (repeat index) (sequence:iterator-step view iterator from-end))
    (sequence:iterator-element view iterator)))
