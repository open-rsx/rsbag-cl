;;;; channel.lisp --- Unit tests for the channel class.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.test)

(deftestsuite channel-root (root)
  ()
  (:documentation
   "Unit test suite for the `channel' class."))

(addtest (channel-root
          :documentation
          "Test printing a `channel' instance.")
  print

  (with-mock-bag (bag :direction :input) (simple-channels)
    (let+ (((first second) (bag-channels bag)))
      (ensure-same "CHANNEL \"/bar\" (3)" (princ-to-string first)
                   :test #'search)
      (ensure-same "CHANNEL \"/foo\" (2)" (princ-to-string second)
                   :test #'search))))
