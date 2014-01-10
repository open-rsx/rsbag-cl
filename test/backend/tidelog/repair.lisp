;;;; repair.lisp --- Unit tests for the repair functions of the tidelog backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog.test)

(addtest (backend-tidelog-root
          :documentation
          "Smoke test for the `reconstruct-indices' function.")
  reconstruct-indices/smoke

  (ensure-cases (input chunks expected)
      `(((,@(tide-block)
          ,@(valid-chnk-block
             :count 1 :content `(,@(chunk-entry))))
         ((0 . 22))
         ((0 1))))

    (let ((stream (apply #'octet-streamify input)))
      (ensure-same (mapcar (lambda (indx)
                             (list (indx-channel-id indx)
                                   (indx-count indx)))
                           (reconstruct-indices stream chunks))
                   expected))))

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
