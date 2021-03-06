;;;; bag.lisp --- Unit tests for the bag class.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
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
          "Smoke test for the `bag-channel' method on `bag' class.")
  bag-channel

  ;; Error signaling and replacement return value.
  (with-mock-bag (bag :direction :input) ()
    (ensure-condition no-such-channel (bag-channel bag "no-such-channel"))
    (ensure-same (bag-channel bag "no-such-channel" :if-does-not-exist nil) nil))

  ;; Creating the channel.
  (with-mock-bag (bag :direction :output) ()
    (let ((channel (bag-channel
                    bag "created"
                    :if-does-not-exist (lambda (condition)
                                         (declare (ignore condition))
                                         (invoke-restart 'create '())))))
      (ensure-same channel (bag-channel bag "created") :test #'eq))))

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

(addtest (bag-root
          :documentation
          "Smoke test for `start-timestamp' and `end-timestamp'
           methods on `bag' class.")
  start+end-timestamp

  (with-mock-bag (bag :direction :input) (simple-channels)
    (start-timestamp bag)
    (end-timestamp bag)))
