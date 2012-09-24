;;; synchronized-bag.lisp --- A bag that synchronizes accesses.
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

(cl:in-package :rsbag)

(defclass synchronized-bag (bag)
  ((lock :reader   %bag-lock
	 :initform (bt:make-lock "Bag lock")
	 :documentation
	 "The lock that is used to synchronized accesses to the
bag."))
  (:documentation
   "Instances of this bag class can be safely used from multiple
threads. Callers have to be prepared to encounter increased latencies
in comparison to the single-threaded case."))

(macrolet
    ((define-synchronized-method (name args)
       `(defmethod ,name :around ,args
	  (bt:with-lock-held ((%bag-lock bag))
	    (call-next-method)))))
  (define-synchronized-method
      close ((bag synchronized-bag)
	     &key &allow-other-keys))
  (define-synchronized-method
      bag-channels ((bag synchronized-bag)))
  (define-synchronized-method
      bag-channel ((bag synchronized-bag)
		    (name t)
		    &key &allow-other-keys))
  (define-synchronized-method
      (setf bag-channel) ((new-value t)
			  (bag      synchronized-bag)
			  (name      t)
			  &key &allow-other-keys)))

(defmethod %channel-class ((bag synchronized-bag))
  (find-class 'synchronized-channel))

(defmethod %make-channel ((bag       synchronized-bag)
			  (name      string)
			  (meta-data list)
			  (transform t)
			  &rest args &key)
  (apply #'call-next-method
	 bag name meta-data transform
	 (append (list :lock (%bag-lock bag)) args)))
