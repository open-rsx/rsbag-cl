;;;; repair.lisp --- Unit tests for the repair functions of the tidelog backend.
;;;;
;;;; Copyright (C) 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog.test)

(defclass mock-index ()
  ((channel-id :initarg :channel-id
               :accessor mock-index-channel-id)
   (entries    :accessor mock-index-entries
               :initform '())))

(defmethod index-add-entries ((index   mock-index)
                              (entries sequence)
                              (chunks  vector))
  (push (cons entries chunks) (mock-index-entries index)))

(addtest (backend-tidelog-root
          :documentation
          "Smoke test for the `reconstruct-indices' function.")
  reconstruct-indices/smoke

  (ensure-cases (input chunks expected)
      `(;; Minimal input
        ((,@(tide-block)
          ,@(valid-chnk-block
             :count 1 :content `(,@(chunk-entry))))
         ,#((0 . 22))
         ((0 ((#() . ,#((0 . 22))))))))

    (let+ ((stream (apply #'octet-streamify input))
           (indices (make-hash-table))
           ((&flet ensure-index (channel-id)
              (ensure-gethash channel-id indices
                              (make-instance 'mock-index
                                             :channel-id channel-id)))))
      (reconstruct-indices stream chunks #'ensure-index)
      (ensure-same (mapcar (lambda (index)
                             (list (mock-index-channel-id index)
                                   (mock-index-entries index)))
                           (hash-table-values indices))
                   expected
                   :test #'equalp))))

(addtest (backend-tidelog-root
          :documentation
          "Smoke test for the `find-next-block' function.")
  find-next-block/smoke

  (ensure-cases (input expected)
      (let ((chnk 'rsbag.backend.tidelog::chnk))
        (append
         ;; Some inputs which are too short to contain a block
         ;; tag. Covers length 4 without tag: 00 00 00 00.
         (iter (for length :to 4)
               (collect `((,(nibbles:make-octet-vector length)) ())))
         ;; For inputs of length 4 or more, place the block tag in all
         ;; possible positions. Covers length 4 with tag: CHNK.
         (iter outer
               (for length :from 4 :to 9)
               (iter (for position :from 0 :to (- length 4))
                     (let ((input (list (nibbles:make-octet-vector position)
                                        :chnk
                                        (nibbles:make-octet-vector (- length 4 position)))))
                       (in outer (collect `(,input ((,position ,chnk))))))))
         ;; Some specific cases of multiple block tags.
         `(((:chnk :chnk)     ((0 ,chnk) (4 ,chnk)))
           ((:chnk 0 :chnk)   ((0 ,chnk) (5 ,chnk)))
           ((0 :chnk :chnk)   ((1 ,chnk) (5 ,chnk)))
           ((0 :chnk 0 :chnk) ((1 ,chnk) (6 ,chnk))))))

    (let* ((stream (apply #'octet-streamify input))
           (result (iter (for offset-and-block next (multiple-value-list
                                                     (find-next-block stream)))
                         (while offset-and-block)
                         (collect offset-and-block)
                         (file-position stream (+ (file-position stream) 4)))))
      (ensure-same result expected :test #'equalp))))
