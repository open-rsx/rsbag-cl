;;; util.lisp --- Utility functions for the TIDELog backend.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsbag.backend.tidelog)


;;; IO-related utility functions
;;

(defun read-chunk-of-length (length stream
			     &optional
			     (buffer (nibbles:make-octet-vector length)))
  "Create an octet-vector (unless BUFFER is supplied) of length LENGTH
and read LENGTH from STREAM into it. Return the octet-vector."
  (let ((read (read-sequence buffer stream)))
    (unless (= read length)
      (error "~@<Could only read ~:D byte~:P when trying to read a ~
sequence of ~:D byte~:P at stream position ~:D.~@:>"
	     read length (file-position stream))))
  buffer)


;;; Time-related utility functions
;;

(defun uint64->timestamp (value)
  (let+ (((&values secs nsecs) (truncate value 1000000000)))
    (local-time:unix-to-timestamp secs :nsec nsecs)))

(defun timestamp->uint64 (value)
  (let+ (((&accessors-r/o (secs  local-time:timestamp-to-unix)
			  (nsecs local-time:nsec-of)) value))
    (+ (* (expt 10 9) secs) nsecs)))
