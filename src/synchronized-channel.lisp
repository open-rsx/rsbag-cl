;;; synchronized-channel.lisp --- A channel that synchronizes accesses.
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

(defclass synchronized-channel (channel)
  ((lock :initarg  :lock
	 :accessor %channel-lock
	 :documentation
	 "The lock that is used to synchronize accesses to the
channel. Usually points to a lock owned by the containing bag."))
  (:documentation
   "Instances of this channel class can be safely used from multiple
threads. Callers have to be prepared to encounter increased latencies
in comparison to the single-threaded case."))

(macrolet
    ((define-synchronized-method (name args)
       `(defmethod ,name :around ,args
		   (bt:with-lock-held ((%channel-lock channel))
		     (call-next-method)))))
  (define-synchronized-method
      channel-timestamp ((channel synchronized-channel)))
  (define-synchronized-method
      entry ((channel synchronized-channel)
	     (index   t)
	     &key &allow-other-keys))
  (define-synchronized-method
      (setf entry) ((new-value t)
		    (channel   synchronized-channel)
		    (index     t)
		    &key &allow-other-keys)))
