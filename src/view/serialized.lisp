;;;; serialized.lisp --- Serialized view on data from multiple channels.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.view)

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
  (lambda (sequence iterator limit from-end)
    (unless (sequence:iterator-endp sequence iterator limit from-end)
      (sequence:iterator-element sequence iterator))))

(defmethod %make-key-function ((sequence channel))
  "When SEQUENCE is a `channel', we can use timestamps as keys by
   using the index of the iterator and looking up the corresponding
   timestamp in `channel-timestamps'."
  (lambda (sequence iterator limit from-end)
    (unless (sequence:iterator-endp sequence iterator limit from-end)
      (sequence:elt
       (channel-timestamps sequence)
       (sequence:iterator-index sequence iterator)))))

(defmethod %make-key-function ((sequence channel-items))
  "When SEQUENCE is of type `channel-items', we can use the index of
   the iterator and look up the corresponding timestamp in the
   timestamp sequence."
  (lambda (sequence iterator limit from-end)
    (unless (sequence:iterator-endp sequence iterator limit from-end)
      (sequence:elt
       (rsbag::channel-items-%timestamps sequence)
       (sequence:iterator-index sequence iterator)))))

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
  (let+ (((&accessors-r/o (sequences view-sequences)
                          (compare   view-compare)
                          (key       view-key)) view)
         ((&flet make-iterator (sequence)
            (let+ (((&values iterator limit from-end)
                    (sequence:make-simple-sequence-iterator
                     sequence :from-end from-end)))
              (list (funcall key sequence iterator limit from-end)
                    sequence iterator limit from-end))))
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
  ((current :initarg  :current
            :accessor iterator-%current
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
                                     compare)
  (setf (iterator-%current instance)
        (%iterator-for-forward-step (iterator-%iterators instance) compare)))

(defmethod sequence:iterator-endp ((sequence serialized)
                                   (iterator serialized-iterator)
                                   (limit    t)
                                   (from-end t))
  (or (null (first (iterator-%current iterator)))
      (call-next-method)))

(defmethod sequence:iterator-step ((sequence serialized)
                                   (iterator serialized-iterator)
                                   (from-end t))
  (let+ (((&accessors-r/o (compare view-compare)
                          (key     view-key)) sequence)
         ((&accessors-r/o (iterators iterator-%iterators)) iterator))
    (declare (type function compare key))
    ;; Step the appropriate sub-iterator (depending on forward
    ;; vs. backward step), then find and store the next sub-iterator.
    (%iterator-step (if from-end
                        (%iterator-for-backward-step iterators key compare)
                        (iterator-%current iterator))
                    key from-end)
    (setf (iterator-%current iterator)
          (%iterator-for-forward-step iterators compare)))
  iterator)

(defmethod sequence:iterator-element ((sequence serialized)
                                      (iterator serialized-iterator))
  (let+ (((&accessors-r/o
           ((&ign sequence* iterator* &rest &ign) iterator-%current)) iterator))
    (sequence:iterator-element sequence* iterator*)))

;;; Utility functions

(declaim (inline %iterator-step)
         (ftype (function (list function boolean) list) %iterator-step))

(defun+ %iterator-step ((&whole state &ign sequence iterator &ign from-end*) key from-end)
  "Destructively perform a step with iterator STATE and update its
   sorting key using KEY. Return the modified STATE."
  ;; Update the iterator and its sorting key.
  (setf (third state) (sequence:iterator-step
                       sequence iterator (xor from-end from-end*))
        (first state) (apply key (rest state)))
  state)

(declaim (inline %iterator-min)
         (ftype (function (list list function) list) %iterator-min))

(defun %iterator-min (left right compare)
  (cond
    ((null (first left))
     right)
    ((null (first right))
     left)
    ((funcall compare (first left) (first right))
     left)
    (t
     right)))

(declaim (inline %iterator-for-forward-step)
         (ftype (function (list function) (or null list))
                %iterator-for-forward-step))

(defun %iterator-for-forward-step (iterators compare)
  "Return the iterator in ITERATORS that should be used to retrieve
   the next element of the serialized sequence or nil."
  (when iterators
    (reduce (rcurry #'%iterator-min compare) iterators)))

(declaim (ftype (function (list function function) (or null list))
                %iterator-for-backward-step))

(defun %iterator-for-backward-step (iterators key compare)
  "Return the iterator in ITERATORS that should be used to retrieve
   the previous element of the serialized sequence of nil."
  (let+ (((&flet cannot-step-back? (iterator)
            (when-let ((index (sequence:iterator-index
                               (second iterator) (third iterator))))
              (zerop index))))
         ((&flet back (iterator)
            (cons (%iterator-step (copy-list iterator) key t) iterator))))
    (cdr (reduce
          (lambda (left right)
            (if (eq (%iterator-min (car left) (car right) compare) (car left))
                right left))
          (mapcar #'back (remove-if #'cannot-step-back? iterators))))))
