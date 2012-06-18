;;; timestamp-adjustment-mixin.lisp --- Unit tests for the timestamp-adjustment-mixin class.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rsbag.rsb.test)

(deftestsuite timestamp-adjustment-mixin-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `timestamp-adjustment-mixin' replay strategy
class."))

(addtest (timestamp-adjustment-mixin-root
          :documentation
	  "Test construction of `timestamp-adjustment-mixin' instances.")
  construction

  (ensure-cases (args expected)
      `(;; These are OK
	(()                                                        t)
	((:adjustments ())                                         t)
	((:adjustments ((:create :now)))                           t)
	((:adjustments ((:create ,(local-time:now))))              t)
	((:adjustments ((:create ,(local-time:now)) (:send :now))) t)
	((:adjustments ((:send   (:copy :create))))                t)

	;; invalid syntax
	((:adjustments :foo)                                       :error) 
	((:adjustments (:foo))                                     :error)
	((:adjustments ((:foo)))                                   :error)
	((:adjustments ((:create (:copy))))                        :error))

    (if (eq expected :error)
	(ensure-condition 'error
	  (apply #'make-instance 'timestamp-adjustment-mixin args))
	(apply #'make-instance 'timestamp-adjustment-mixin args))))
