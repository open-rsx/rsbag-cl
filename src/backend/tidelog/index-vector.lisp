;;;; index-vector.lisp --- Data structure for index entries.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

(deftype index-vector ()
  '(array (unsigned-byte 64) (*)))

(declaim (inline make-index-vector
                 index-vector-length
                 index-vector-push-entry
                 index-vector-push-extend-entry
                 index-vector-index->offset
                 index-vector-index->timestamp
                 index-vector-timestamp->offset))

(defun make-index-vector ()
  (make-array 0 :element-type '(unsigned-byte 64)
                :adjustable   t
                :fill-pointer 0))

(defun index-vector-length (vector)
  (/ (length vector) 2))

(defun index-vector-push-entry (vector timestamp offset)
  (vector-push timestamp vector)
  (vector-push offset    vector))

(defun index-vector-push-extend-entry (vector timestamp offset)
  (vector-push-extend timestamp vector (floor (length vector) 8/2)) ; has to be even
  (vector-push        offset    vector))

(defun index-vector-add-entries (vector entries chunks)
  (declare (type index-vector vector))
  (let+ ((num-entries (length entries))
         ((&flet add-offset! (entry)
            (let+ (((&structure-r/o index-entry- timestamp chunk-id offset)
                    entry)
                   (outer-offset  (%chunk-id->offset chunk-id chunks))
                   (global-offset (+ outer-offset 12 25 offset))) ; TODO(jmoringe):  get rid of the constants
              (index-vector-push-entry vector timestamp global-offset)))))
    (adjust-array vector (+ (length vector) (* 2 num-entries)))
    (map nil #'add-offset! entries)
    vector))

(defun index-vector-add-indxs (vector indxs chunks)
  (declare (type index-vector vector))
  (let ((num-entries (reduce #'+ indxs :key #'indx-count)))
    (adjust-array vector (+ (length vector) (* 2 num-entries)))
    (iter (for indx in-sequence indxs)
          (index-vector-add-entries vector (indx-entries indx) chunks))
    vector))

(defun index-vector-index->offset (index vector)
  (aref vector (1+ (* 2 index))))

(defun index-vector-index->timestamp (index vector)
  (aref vector (* 2 index)))

(defun index-vector-timestamp->offset (timestamp vector)
  (%timestamp->index timestamp vector))

;;; Utilities

(declaim (ftype (function ((unsigned-byte 64) index-vector)
                          (values (unsigned-byte 64) &optional))
                %timestamp->index))
(defun %timestamp->index (timestamp vector)
  (labels ((rec (start end)
             (declare (type array-index start end))
             (let* ((pivot  (ash (+ start end) -1))
                    (pivot* (aref vector (* 2 pivot))))
               (cond
                 ((< pivot* timestamp) (rec pivot end))
                 ((> pivot* timestamp) (rec start pivot))
                 (t                    (aref vector (1+ (* 2 pivot))))))))
    (rec 0 (length vector))))
