;;;; bag.lisp --- Unit tests for the bag class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.test)

(deftestsuite bag-root (root)
  ()
  (:documentation
   "Unit test suite for the `bag' class."))

(addtest (bag-root
          :documentation
	  "Smoke test for the `bag' class.")
  smoke

  (let* ((pathname (asdf:system-relative-pathname
		    :cl-rsbag-test "test/data/minimal.tide"))
	 (bag      (handler-bind
		       ((open-error #'continue))
		     (open-bag pathname :direction :input))))
    (ensure-same (mapcar #'channel-name (bag-channels bag))
		 '("MYCHAN")
		 :test #'equal)
    (let ((channel (first (bag-channels bag))))
      (ensure-same (length (channel-timestamps channel)) 1
		   :test #'=)
      (ensure-same (coerce channel 'list) '(#(1 2 3))
		   :test #'equalp))
    (close bag)))
