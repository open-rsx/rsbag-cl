;;; serialized.lisp --- Unit tests for the serialized view class.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsbag.view.test)

(deftestsuite serialized-root (view-root)
  ((one-sequence  '(((0 :a) (1 :b) (2 :c))))
   (two-sequences `(((0 :a)        (2 :c))
		    (       (1 :b)        (3 :d)))))
  (:documentation
   "Test suite for the `serialized' view class."))

(addtest (serialized-root
          :documentation
	  "Smoke test for the `serialized' view class.")
  smoke

  (ensure-cases (sequences start end expected-length expected-items)
      `((nil            0 0 0 nil)
	(,one-sequence  0 0 3 ((0 :a) (1 :b) (2 :c)))
	(,one-sequence  1 0 3 (       (1 :b) (2 :c)))
	(,two-sequences 0 0 4 ((0 :a) (1 :b) (2 :c) (3 :d))))
    (let ((sequence (make-instance
		     'serialized
		     :sequences sequences
		     :compare   #'<
		     :key       (lambda (sequence iterator limit from-end)
				  (first (sequence:iterator-element sequence iterator))))))
      (ensure-same (length sequence) expected-length)
      (iter (for (timestamp   value)   each  sequence       :from start)
	    (for (e-timestamp e-value) each  expected-items)
	    (for i                     :from start)
	    (for (timestamp*  value*)  next  (elt sequence i))
	    (ensure-same timestamp e-timestamp
			 :test #'=)
	    (ensure-same timestamp* e-timestamp
			 :test #'=)
	    (ensure-same value e-value
			 :test #'eq)
	    (ensure-same value* e-value
			 :test #'eq)))))
