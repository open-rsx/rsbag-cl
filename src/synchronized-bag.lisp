;;; synchronized-bag.lisp --- A bag that synchronizes accesses.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsbag)

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
			  &optional
			  id)
  (bind (((:accessors-r/o (backend       %bag-backend)
			  (channel-class %channel-class)) bag))
    (make-instance channel-class
		   :bag     bag
		   :name    name
		   :id      (or id (make-channel-id backend name))
		   :backend backend
		   :lock    (%bag-lock bag))))
