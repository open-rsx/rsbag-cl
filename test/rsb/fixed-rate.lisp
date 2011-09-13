;;; fixed-rate.lisp --- Unit tests for the fixed-rate class.
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

(in-package :rsbag.rsb.test)

(deftestsuite fixed-rate-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `fixed-rate' replay strategy class."))

(addtest (fixed-rate-root
          :documentation
	  "Test construction of `fixed-rate' instances.")
  construction

  (ensure-cases (args)
      '(()
	(:delay 1 :rate  1)
	(:delay 0)
	(:rate 0))

    (ensure-condition 'error
      (apply #'make-instance 'fixed-rate args))))
