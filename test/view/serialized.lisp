;;;; serialized.lisp --- Unit tests for the serialized view class.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.view.test)

(deftestsuite serialized-root (view-root)
  ((one-sequence  '(((0 :a) (1 :b) (2 :c))))
   (two-sequences '(((0 :a)        (2 :c))
                    (       (1 :b)        (3 :d)))))
  (:documentation
   "Test suite for the `serialized' view class."))

(addtest (serialized-root
          :documentation
          "Smoke test for the `serialized' view class.")
  smoke

  (ensure-cases (sequences start end expected-length expected-items)
      `((nil            0 0 0 nil)
        (,one-sequence  0 0 3 ((0 :a) (1 :b) (2 :c)))
        (,one-sequence  1 0 3 (       (1 :b) (2 :c)))
        (,two-sequences 0 0 4 ((0 :a) (1 :b) (2 :c) (3 :d))))

    (let ((sequence (make-instance
                     'serialized
                     :sequences sequences
                     :compare   #'<
                     :key       (lambda (sequence iterator)
                                  (first (sequence:iterator-element
                                          sequence iterator))))))
      (ensure-same (length sequence) expected-length)
      (iter (for (timestamp   value)   each  sequence       :from start)
            (for (e-timestamp e-value) each  expected-items)
            (for i                     :from start)
            (for (timestamp*  value*)  next  (elt sequence i))
            (ensure-same timestamp  e-timestamp :test #'=)
            (ensure-same timestamp* e-timestamp :test #'=)
            (ensure-same value      e-value     :test #'eq)
            (ensure-same value*     e-value     :test #'eq)))))

(addtest (serialized-root
          :documentation
          "Test constructing `serialized' view instances using
           `make-serialized-view'.")
  construction

  (ensure-cases (sequences expected-length)
      `((nil            0)
        (,one-sequence  3)
        (,two-sequences 4))

    (let ((sequence (make-serialized-view sequences)))
      (ensure-same (length sequence) expected-length))))


(addtest (serialized-root
          :documentation
          "Test forward and backward iteration on a serialized view on
           multiple random sequences.")
  iterator

  (let+ (((&flet make-random-sequence ()
            (let ((raw (map-into (make-list (random 100)) (curry #'random 100))))
             (sort (remove-duplicates raw) #'<))))
         ((&flet make-random-sequences ()
            (let ((count (random 10)))
              (map-into (make-list count) #'make-random-sequence)))))
    (ensure-cases (sequences)
        (map-into (make-list 100) #'make-random-sequences)

      (let+ ((flat (sort (copy-list (reduce #'append sequences)) #'<))
             (view (make-serialized-view sequences :compare #'<))
             ((&values iterator &ign from-end)
              (sequence:make-simple-sequence-iterator view))
             ((&flet traverse (backward?)
                (iter (repeat (length view))
                      (unless (first-iteration-p)
                        (setf iterator (sequence:iterator-step
                                        view iterator (xor backward? from-end))))
                      (collect (sequence:iterator-element view iterator)))))
             (forward  (traverse nil))
             (backward (traverse t)))
        (ensure-same forward flat
                     :test   #'equal
                     :report "Forward iteration failed")
        (ensure-same (reverse backward) flat
                     :test   #'equal
                     :report "Backward iteration failed")))))
