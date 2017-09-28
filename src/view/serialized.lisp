;;;; serialized.lisp --- Serialized view on data from multiple channels.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.view)

;;; In-parallel steppable iterator states

(defstruct (%iterator
             (:constructor make-%iterator
                           (key sequence iterator limit from-end))
             (:predicate nil))
  ;; The comparison key for the sequence element at which the iterator
  ;; is currently positioned.
  (key      nil)
  ;; Iterator state parts: sequence through which the iterator
  ;; iterates, iterator state, iteration limit and from-end flag.
  (sequence nil :read-only t)
  (iterator nil)
  (limit    nil :read-only t)
  (from-end nil :read-only t))

(declaim (inline %iterator-step)
         (ftype (function (%iterator function boolean)
                          (values %iterator &optional))
                %iterator-step))
(defun %iterator-step (state key from-end)
  "Destructively perform a step with iterator STATE and update its
   sorting key using KEY. Return the modified STATE."
  ;; Update the iterator and its sorting key.
  (let+ (((&structure
           %iterator- (key* key) sequence iterator limit (from-end* from-end))
          state))
    (setf iterator
          (sequence:iterator-step sequence iterator (xor from-end from-end*))
          key*
          (unless (sequence:iterator-endp
                   sequence iterator limit (xor from-end from-end*))
            (funcall key sequence iterator))))
  state)

(declaim (inline %iterator<)
         (ftype (function (%iterator %iterator function)
                          (values boolean &optional))
                %iterator<))
(defun %iterator< (left right compare)
  (cond
    ((null (%iterator-key left))
     nil)
    ((null (%iterator-key right))
     t)
    ((funcall compare (%iterator-key left) (%iterator-key right))
     t)
    (t
     nil)))

(declaim (ftype (function (list function function)
                          (values (or null %iterator) &optional))
                %iterator-for-backward-step))
(defun %iterator-for-backward-step (iterators key compare)
  "Return the iterator in ITERATORS that should be used to retrieve
   the previous element of the serialized sequence of nil."
  (let+ (((&flet cannot-step-back? (iterator)
            (let+ (((&structure-r/o %iterator- sequence iterator) iterator))
              (when-let ((index (sequence:iterator-index sequence iterator)))
                (zerop index)))))
         ((&flet back (iterator)
            (cons (%iterator-step (copy-%iterator iterator) key t) iterator))))
    (cdr (reduce
          (lambda (left right)
            (if (%iterator< (car left) (car right) compare) right left))
          (mapcar #'back (remove-if #'cannot-step-back? iterators))))))

;;; Construction methods

(defmethod make-serialized-view ((sequences bag)
                                 &key
                                 (selector #'identity)
                                 (compare  #'local-time:timestamp<))
  "Create a serialized view for the channels of a bag."
  (make-serialized-view (bag-channels sequences)
                        :selector selector
                        :compare  compare))

(defmethod make-serialized-view ((sequences sequence)
                                 &key
                                 (selector #'identity)
                                 (compare  #'local-time:timestamp<) )
  (let* ((transformed (map 'list selector sequences))
         (key         (if transformed
                          (%make-key-function (first transformed))
                          #'identity)))
    (make-instance 'serialized
                   :sequences transformed
                   :key       key
                   :compare   compare)))

;;; Key creation methods

(defmethod %make-key-function ((sequence sequence))
  "When SEQUENCE is just a `sequence', we assume it consists of
   timestamps."
  (lambda (sequence iterator)
    (sequence:iterator-element sequence iterator)))

(defmethod %make-key-function ((sequence channel))
  "When SEQUENCE is a `channel', we can use timestamps as keys by
   using the index of the iterator and looking up the corresponding
   timestamp in `channel-timestamps'."
  (lambda (sequence iterator)
    (sequence:elt
     (channel-timestamps sequence)
     (sequence:iterator-index sequence iterator))))

(defmethod %make-key-function ((sequence channel-items))
  "When SEQUENCE is of type `channel-items', we can use the index of
   the iterator and look up the corresponding timestamp in the
   timestamp sequence."
  (lambda (sequence iterator)
    (sequence:elt
     (rsbag::channel-items-%timestamps sequence)
     (sequence:iterator-index sequence iterator))))

;;; `serialized' class

(defclass serialized (multi-sequence-view-mixin
                      elt-via-iterator-mixin
                      sequence)
  ((compare :initarg  :compare
            :type     function
            :accessor view-compare
            :initform #'local-time:timestamp<
            :documentation
            "Stores a function that is used to compare keys extracted
             from iterator states in order to decide which iterator
             has to be stepped.")
   (key     :initarg  :key
            :type     function
            :accessor view-key
            :documentation
            "Stores a function that extracts keys from iterator states
             which are used to decide which iterator has to be
             stepped."))
  (:default-initargs
   :key (missing-required-initarg 'serialized :key))
  (:documentation
   "Instances of this class provide the data of multiple channels as a
    single sequence in which items from different channels are
    serialized according to their timestamps."))

(defmethod sequence:length ((view serialized))
  ;; The number of events is the sums of the numbers of events of
  ;; individual channels.
  (reduce #'+ (view-sequences view) :key #'length))

(defmethod sequence:make-simple-sequence-iterator ((view serialized)
                                                   &key
                                                   from-end
                                                   (start   0)
                                                   end)
  (let+ (((&structure-r/o view- sequences compare key) view)
         ((&flet make-iterator (sequence)
            (let+ (((&values iterator limit from-end)
                    (sequence:make-simple-sequence-iterator
                     sequence :from-end from-end))
                   (key (unless (sequence:iterator-endp
                                 sequence iterator limit from-end)
                          (funcall key sequence iterator))))
              (make-%iterator key sequence iterator limit from-end))))
         ;; Build the iterator, omitting empty sequences for
         ;; simplicity and efficiency.
         (iterator (make-instance
                    'serialized-iterator
                    :iterators (map 'list #'make-iterator
                                    (remove-if #'emptyp sequences))
                    :compare   compare)))
    (iter (repeat start)
          (setf iterator (sequence:iterator-step view iterator from-end)))
    (values iterator (or end (length view)) from-end)))

;;; `serialized-iterator' class

(defclass serialized-iterator (multi-sequence-iterator-mixin)
  ((sorted  :type     pileup:heap
            :accessor iterator-%sorted
            :documentation
            "Stores the set of iterators sorted with respect to the
             comparison function and their respective key.")
   (current :accessor iterator-%current
            :documentation
            "Stores the iterator that holds the current element and
             has to be stepped in order to step in the serialized
             view."))
  (:documentation
   "Instances of this class perform iterations through sequences that
    are serialized views on multiple sequences."))

(defmethod shared-initialize :after ((instance   serialized-iterator)
                                     (slot-names t)
                                     &key
                                     iterators
                                     compare)
  (declare (type function compare))
  (let+ (((&structure iterator-% sorted current) instance)
         ((&flet predicate (left right)
            (%iterator< left right compare))))
    (setf sorted (pileup:make-heap #'predicate))
    (map nil (rcurry #'pileup:heap-insert sorted) iterators)
    (setf current (pileup:heap-top sorted))))

(defmethod sequence:iterator-endp ((sequence serialized)
                                   (iterator serialized-iterator)
                                   (limit    t)
                                   (from-end t))
  (or (when-let ((current (iterator-%current iterator)))
        (null (%iterator-key current)))
      (call-next-method)))

(defmethod sequence:iterator-step ((sequence serialized)
                                   (iterator serialized-iterator)
                                   (from-end t))
  (let+ (((&structure-r/o view- compare key) sequence)
         ((&structure iterator-% iterators sorted current) iterator)
         (to-be-stepped
          (if from-end
              (%iterator-for-backward-step iterators key compare)
              (pileup:heap-pop sorted))))
    (declare (type function compare key))
    ;; Step the appropriate sub-iterator (depending on forward
    ;; vs. backward step), then find and store the next sub-iterator.
    (%iterator-step to-be-stepped key from-end)
    (if from-end
        (progn
          (setf sorted (pileup:make-heap (pileup:heap-predicate sorted)))
          (map nil (rcurry #'pileup:heap-insert sorted) iterators))
        (pileup:heap-insert to-be-stepped sorted))
    (setf current (pileup:heap-top sorted)))
  iterator)

(defmethod sequence:iterator-element ((sequence serialized)
                                      (iterator serialized-iterator))
  (let+ (((&structure-r/o %iterator- sequence iterator)
          (iterator-%current iterator)))
    (sequence:iterator-element sequence iterator)))
