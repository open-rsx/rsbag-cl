;;; bag.lisp --- Unit tests for the bag class.
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

(cl:in-package :rsbag.test)

(deftestsuite bag-root (root)
  ()
  (:documentation
   "Unit test suite for the `bag' class."))

(addtest (bag-root
          :documentation
	  "Smoke test for the `bag' class.")
  smoke

  (let* ((pathname (asdf:system-relative-pathname
		    :cl-rsbag-test "test/data/minimal.tide"))
	 (bag      (handler-bind
		       ((open-error #'continue))
		     (open-bag pathname :direction :input))))
    (ensure-same (mapcar #'channel-name (bag-channels bag))
		 '("MYCHAN")
		 :test #'equal)
    (let ((channel (first (bag-channels bag))))
      (ensure-same (length (channel-timestamps channel)) 1
		   :test #'=)
      (ensure-same (coerce channel 'list) '(#(1 2 3))
		   :test #'equalp))
    (close bag)))
