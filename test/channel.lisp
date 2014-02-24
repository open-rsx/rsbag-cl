;;;; channel.lisp --- Unit tests for the channel class.
;;;;
;;;; Copyright (C) 2014, 2016 Jan Moringen
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

(addtest (channel-root
          :documentation
          "Test errors signaled by attempts to modify `channel'
           instances in read-only `bag's.")
  read-only

  (with-mock-bag (bag :direction :input) (simple-channels)
    (ensure-condition direction-error
      (setf (entry (first (bag-channels bag)) (local-time:now))
            :does-not-matter))))

(addtest (channel-root
          :documetation
          "Smoke test for `start-timestamp' and `end-timestamp'
           methods on `channel' class.")
  start+end-timestamp

  (with-mock-bag (bag :direction :input) (simple-channels)
    (let ((channel (first (bag-channels bag))))
     (start-timestamp channel)
     (end-timestamp channel))))
