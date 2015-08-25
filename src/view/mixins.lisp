;;;; elt-via-iterator-mixin.lisp --- Mixin class for non-random-access sequences
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.view)

;;; `elt-via-iterator-mixin' class

(defclass elt-via-iterator-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into sequence classes that
    cannot provide `sequence:elt' directly but can provide
    iterators. Subclasses inherit a method on `sequence:elt' that
    positions an iterator on the requested index and retrieves the
    element from it."))

(defmethod sequence:elt ((view  elt-via-iterator-mixin)
                         (index integer))
  ;; Create an iterator and advance it to INDEX.
  (let+ (((&values iterator &ign from-end)
          (sequence:make-simple-sequence-iterator view)))
    (iter (repeat index) (sequence:iterator-step view iterator from-end))
    (sequence:iterator-element view iterator)))

;;; `multi-sequence-view-mixin' class

(defclass multi-sequence-view-mixin ()
  ((sequences :initarg  :sequences
              :type     list
              :reader   view-sequences
              :documentation
              "Stores the list of sequences from the view aggregates
               data."))
  (:default-initargs
   :sequences (missing-required-initarg 'multi-sequence-view-mixin :sequences))
  (:documentation
   "This class is intended to be mixed into view classes that
    aggregate data from multiple sequences."))

(defmethod print-object ((object multi-sequence-view-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (view-sequences object)))))

;;; `multi-sequence-iterator-mixin' class

(defclass multi-sequence-iterator-mixin ()
  ((iterators :initarg  :iterators
              :type     list
              :reader   iterator-%iterators
              :documentation
              "Stores iterators that represent sequence-specific
               iteration states.")
   (index     :initarg  :index
              :type     non-negative-integer
              :accessor iterator-%index
              :initform 0
              :documentation
              "Stores the current index of the iteration."))
  (:default-initargs
   :iterators (missing-required-initarg 'multi-sequence-iterator-mixin :iterators))
  (:documentation
   "This class is intended to be mixed into iterator classes that
    represent the state of iterations which span multiple
    sequences."))

(defmethod sequence:iterator-step :after ((sequence sequence)
                                          (iterator multi-sequence-iterator-mixin)
                                          (from-end t))
  (incf (iterator-%index iterator) (if from-end -1 1)))

(defmethod sequence:iterator-endp ((sequence sequence)
                                   (iterator multi-sequence-iterator-mixin)
                                   (limit    t)
                                   (from-end t))
  (= (iterator-%index iterator) limit))

(defmethod sequence:iterator-index ((sequence sequence)
                                    (iterator multi-sequence-iterator-mixin))
  (iterator-%index iterator))

(defmethod print-object ((object multi-sequence-iterator-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D" (iterator-%index object))))
