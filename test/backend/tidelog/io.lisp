;;;; io.lisp --- Unit tests for the block io functions of the tidelog backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog.test)

(addtest (backend-tidelog-root
          :documentation
          "Smoke test for the `scan' method specialized on the TIDE
           block.")
  scan/tide/smoke

  (ensure-cases (input expected)
    `(;; Wrong block.
      (,(valid-chnk-block)                                      error)
      ;; Wrong major version.
      (,(tide-block :version-major (1+ +format-version-major+)) error)
      ;; Valid case.
      (,(tide-block)                                            (() () () t))
      (,(tide-block :version-minor (1+ +format-version-minor+)) (() () () t))
      (,(append (tide-block) (valid-chnk-block))                (() () ((0 . 22)) t)))

    (let+ ((stream (apply #'octet-streamify input))
           ((&flet do-it () (scan stream :tide))))
      (case expected
        (error (ensure-condition 'error (do-it)))
        (t     (ensure-same (do-it) (values-list expected)))))))

(addtest (backend-tidelog-root
          :documentation
          "Test restarts established by the `scan' method specialized
           on the TIDE for dealing with corrupt files.")
  scan/tide/restarts

  (ensure-cases (input restarts expected)
      `(;; Skip once.
        ((,@(tide-block) ,@(valid-chnk-block) ,@+invalid-chnk-block+
          ,@(valid-chnk-block))
         (continue)
         (() () ((0 . 22) (0 . 75)) nil))
        ;; Skip once with garbage at end.
        ((,@(tide-block) ,@(valid-chnk-block) ,@+invalid-chnk-block+
          ,@(valid-chnk-block) :chnk (:ub64le 0))
         (continue continue)
         (() () ((0 . 22) (0 . 75)) nil))
        ;; Skip twice.
        ((,@(tide-block) ,@(valid-chnk-block) ,@+invalid-chnk-block+
          ,@(valid-chnk-block) ,@+invalid-chnk-block+ ,@(valid-chnk-block))
         (continue continue)
         (() () ((0 . 22) (0 . 75) (0 . 128)) nil)))

    (let+ ((stream (apply #'octet-streamify input))
           ((&flet do-it ()
              (multiple-value-prog1
                  (handler-bind
                      ((error (lambda (condition)
                                (invoke-restart
                                 (or (pop restarts)
                                     (error "~@<No more restarts~@:>"))
                                 condition))))
                    (scan stream :tide))
                (unless (null restarts)
                  (error "~@<Leftover restarts: ~S~@:>" restarts))))))
      (ensure-same (do-it) (values-list expected)))))

(addtest (backend-tidelog-root
          :documentation
          "Smoke test for the `byte-pattern->block-class' function.")
  byte-pattern->block-class/smoke

  ;; Try to lookup a non-existing block class.
  (ensure-condition no-such-block-class-error
    (byte-pattern->block-class (octetify "NONO")))

  ;; Lookup an existing block class.
  (ensure-same (find-class 'rsbag.backend.tidelog::tide)
               (byte-pattern->block-class (octetify "TIDE"))))
