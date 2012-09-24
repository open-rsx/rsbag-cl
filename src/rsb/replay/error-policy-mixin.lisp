;;; error-policy-mixin.lisp --- error-policy-mixin mixin class.
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

(cl:in-package :rsbag.rsb.replay)

(defclass error-policy-mixin (rsb.ep:error-policy-mixin)
  ()
  (:default-initargs
   :error-policy #'log-error)
  (:documentation
   "This mixin class provides a method on `replay' that arranges for
the next `replay' methods to be called with error handling based on
the installed error policy."))

(defmethod replay :around ((connection replay-bag-connection)
			   (strategy   error-policy-mixin)
			   &key &allow-other-keys)
  (rsb.ep:with-error-policy (strategy)
    (call-next-method)))
