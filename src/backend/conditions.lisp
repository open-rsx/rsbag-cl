;;; conditions.lisp --- Conditions used in backend modules.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rsbag.backend)

(define-condition log-file-error (rsbag-error)
  ((source :initarg  :source
	   :reader   log-file-error-source
	   :documentation
	   "Stores the source involved in the error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<An error has been encountered when operating ~
on ~A.~@:>"
	     (log-file-error-source condition))))
  (:documentation
   "Errors of this class and subclasses are signaled when operations
involving log files fail."))

(define-condition invalid-file-structure (simple-error
					  log-file-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Invalid file structure encountered in ~
~A~/more-conditions::maybe-print-explanation/~@:>"
	     (log-file-error-source condition)
	     condition)))
  (:documentation
   "This error is signaled if an invalid file structure is encountered
while reading a log file."))
