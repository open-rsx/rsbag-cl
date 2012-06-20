;;; protocol.lisp --- Unit tests for the client-facing protocol.
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

(cl:in-package :rsbag.test)

(deftestsuite protocol-root (root)
  ()
  (:function
   (pathname/existing ()
     (asdf:system-relative-pathname
      (asdf:find-system :cl-rsbag-test)
      "test/data/minimal.tide")))
  (:function
   (namestring/existing ()
     (pathname/existing)))
  (:function
   (stream ()
     (open (pathname/existing)
	   :element-type '(unsigned-byte 8)
	   :direction    :input)))
  (:documentation
   "Unit test for the client-facing protocol."))

(addtest (protocol-root
          :documentation
	  "Test case for the `open-bag' function.")
  open-bag/valid

  (ensure-cases (args)
      `((,(namestring/existing) :direction :input)
	(,(pathname/existing)   :direction :input)
	(,(namestring/existing) :direction :input :backend :tide)
	(,(pathname/existing)   :direction :input :backend :tide)
	(,(stream)              :direction :input :backend :tide))
    (let ((bag (apply #'open-bag args)))
      (close bag))))

(addtest (protocol-root
          :documentation
	  "Test cases for which the `open-bag' function has to signal
errors.")
  open-bag/invalid

  (ensure-cases (args)
      `(;; :backend, :direction missing
	(,(stream))
	;; :backend missing
	(,(stream)              :direction :input)
	;; :direction missing
	(,(stream)              :backend :tide)
	;; invalid direction
	(,(namestring/existing) :direction :invalid :backend :tide)
	(,(pathname/existing)   :direction :invalid :backend :tide)
	(,(stream)              :direction :invalid :backend :tide)
	;; invalid backend
	(,(namestring/existing) :direction :input :backend :no-such-backend)
	(,(pathname/existing)   :direction :input :backend :no-such-backend)
	(,(stream)              :direction :input :backend :no-such-backend)
	;; file exists
	(,(namestring/existing) :direction :output :backend :tide)
	(,(pathname/existing)   :direction :output :backend :tide)
	;; cannot specify flush strategy for input direction
	(,(namestring/existing) :direction :input :flush-strategy :some-strategy)
	(,(pathname/existing)   :direction :input :flush-strategy :some-strategy)
	(,(stream)              :direction :input :backend :tide :flush-strategy :some-strategy)
	;; invalid flush strategy
	(,(namestring/existing) :direction :io :flush-strategy :no-such-strategy)
	(,(pathname/existing)   :direction :io :flush-strategy :no-such-strategy)
	(,(stream)              :direction :io :backend :tide :flush-strategy :no-such-strategy))

    (ensure-condition 'error
      (apply #'open-bag args))))
