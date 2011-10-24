;;; bag.lisp --- Unit tests for the bag class.
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

(in-package :rsbag.test)

(deftestsuite bag-root (root)
  ()
  (:documentation
   "Unit test suite for the `bag' class."))

(addtest (bag-root
          :documentation
	  "Smoke test for the `bag' class.")
  smoke

  (let* ((pathname (asdf:system-relative-pathname
		    (asdf:find-system :cl-rsbag-test)
		    "test/data/minimal.tide"))
	 (bag (open-bag pathname
			:direction :input)))
    (ensure-same (map 'list #'channel-name (bag-channels bag))
		 '("MYCHAN")
		 :test #'equal)
    (let ((channel (first (bag-channels bag))))
      (ensure-same (length (channel-timestamps channel)) 1
		   :test #'=)
      (ensure-same (coerce channel 'list)
		   '(#(1 2 3))
		   :test #'equalp))
    (close bag)))
