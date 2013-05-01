;;;; elt-via-iterator-mixin.lisp --- Mixin class for non-random-access sequences
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.view)

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
  (let+ (((&values iterator &ign from-end)
	  (sequence:make-simple-sequence-iterator view)))
    (iter (repeat index) (sequence:iterator-step view iterator from-end))
    (sequence:iterator-element view iterator)))
