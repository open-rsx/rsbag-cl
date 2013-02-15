;;; util.lisp --- Utility functions for the TIDELog backend.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsbag.backend.tidelog)


;;; IO-related utility functions
;;

(defun read-chunk-of-length (length stream
			     &optional
			     (buffer (nibbles:make-octet-vector length)))
  "Create a `simple-octet-vector' (unless BUFFER is supplied) of
length LENGTH and read LENGTH from STREAM into it. Return the buffer."
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
    (declare (type non-negative-integer     secs)
	     (type (integer 0 (1000000000)) nsecs))
    (+ (* (expt 10 9) secs) nsecs)))
