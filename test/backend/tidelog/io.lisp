;;;; io.lisp --- Unit tests for the block io functions of the tidelog backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog.test)

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
