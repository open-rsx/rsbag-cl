;;; conditions.lisp --- Conditions used in the TIDE log backend of cl-rsbag.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(define-condition tidelog-condition (condition)
  ()
  (:documentation
   "This condition class serves as a superclass for TIDELOG-related
condition classes."))

(define-condition tidelog-file-error (log-file-error
				      tidelog-condition)
  ()
  (:documentation
   "Errors of this class and subclasses are signaled when operations
involving TIDE log files fail."))

(define-condition invalid-tidelog-structure (invalid-file-structure
					     tidelog-condition)
  ()
  (:documentation
   "This error is signaled if an invalid file structure is encountered
while processing a TIDE log file."))
