;;;; protocol.lisp --- Protocol for the view module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.view)

;;; View construction functions

(defgeneric make-serialized-view (sequences
                                  &key
                                  selector
                                  compare)
  (:documentation
   "Make and return a sequence that consists of a serialization of the
elements of SEQUENCES. The serialization is performed by comparing
timestamps of elements arranging elements in the order of increasing
timestamps.

SELECTOR is a function that is applied to each element of SEQUENCES
before the view is constructed. When SEQUENCES is a sequence of
`channel's or a `bag', functions such as `channel-timestamps',
`channel-entries' and `channel-items' can be supplied as SELECTOR to
select the elements of the returned sequence.

COMPARE is a ordering predicate."))

;;; Extensible support functions

(defgeneric %make-key-function (sequence)
  (:documentation
   "Return a function that takes four arguments, a sequence, an
iterator, a limit and a from-end-value, and returns a serialization
key (for example a `local-time:timestamp') for the corresponding
element. The returned function has to be applicable to sequences of
the type of SEQUENCE and associated iterators."))
