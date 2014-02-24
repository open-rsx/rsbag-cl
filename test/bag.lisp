;;;; bag.lisp --- Unit tests for the bag class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.test)

(deftestsuite bag-root (root)
  ()
  (:documentation
   "Unit test suite for the `bag' class."))

(addtest (bag-root
          :documentation
          "Smoke test for the `bag' class.")
  smoke

  (with-mock-bag (bag :direction :input) (simple-channels)

    (ensure-same (bag-direction bag) :input)
    (ensure-same (mapcar #'channel-name (bag-channels bag))
                 '("/bar" "/foo")
                 :test #'equal)

    ;; Check content of first channel.
    (let ((channel (first (bag-channels bag))))
      (ensure-same (length (channel-timestamps channel)) 3)
      (ensure-same (coerce channel 'list) '(3 4 5) :test #'equalp))

    ;; Check content of second channel.
    (let ((channel (second (bag-channels bag))))
      (ensure-same (length (channel-timestamps channel)) 2)
      (ensure-same (coerce channel 'list) '(1 2) :test #'equalp))))

(addtest (bag-root
          :documentation
          "Test printing a `bag' instance.")
  print

  (ensure-cases (direction channels expected)
      `((:input  ()                 "BAG N/A r- (0)")
        (:output ()                 "BAG N/A -w (0)")
        (:io     ()                 "BAG N/A rw (0)")
        (:input  ,(simple-channels) "BAG N/A r- (2)"))

    (with-mock-bag (bag :direction direction) channels
      (ensure-same expected (princ-to-string bag) :test #'search))))

(addtest (bag-root
          :documentation
          "Test errors signaled by attempts to modify read-only `bag'
           instances.")
  read-only

  (with-mock-bag (bag :direction :input) '()
    (ensure-condition direction-error
      (setf (bag-channel bag "foo") '()))))
