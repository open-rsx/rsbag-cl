;;; error-policy-mixin.lisp --- error-policy-mixin mixin class.
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