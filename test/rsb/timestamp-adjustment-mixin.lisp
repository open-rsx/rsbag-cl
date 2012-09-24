;;; timestamp-adjustment-mixin.lisp --- Unit tests for the timestamp-adjustment-mixin class.
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
