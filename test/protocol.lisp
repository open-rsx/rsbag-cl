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
  (:documentation
   "Unit test for the client-facing protocol."))

(addtest (protocol-root
          :documentation
	  "Test case for the `open-bag' function.")
  open-bag

  (let* ((pathname   (asdf:system-relative-pathname
		      (asdf:find-system :cl-rsbag-test)
		      "test/data/minimal.tide"))
	 (namestring (namestring pathname))
	 (stream     (open pathname
			   :element-type '(unsigned-byte 8)
			   :direction    :input)))
    (ensure-cases (args)
	`(;; :backend, :direction missing
	  (,stream)
	  ;; :backend missing
	  (,stream     :direction :io)
	  ;; :direction missing
	  (,stream     :backend :tidelog)
	  ;; invalid direction
	  (,namestring :direction :invalid :backend :tidelog)
	  (,pathname   :direction :invalid :backend :tidelog)
	  (,stream     :direction :invalid :backend :tidelog)
	  ;; invalid backend
	  (,namestring :direction :io :backend :no-such-backend)
	  (,pathname   :direction :io :backend :no-such-backend)
	  (,stream     :direction :io :backend :no-such-backend))

      (ensure-condition 'error
	(apply #'open-bag args)))))
