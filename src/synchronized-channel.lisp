;;; synchronized-channel.lisp --- A channel that synchronizes accesses.
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

(defclass synchronized-channel (channel)
  ((lock :initarg  :lock
	 :accessor %channel-lock
	 :documentation
	 "The lock that is used to synchronize accesses to the
channel. Usually points to a lock owned by the containing bag."))
  (:default-initargs
   :lock (required-argument :lock))
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
      channel-timestamps ((channel synchronized-channel)))
  (define-synchronized-method
      entry ((channel synchronized-channel)
	     (index   t)
	     &key &allow-other-keys))
  (define-synchronized-method
      (setf entry) ((new-value t)
		    (channel   synchronized-channel)
		    (index     t)
		    &key &allow-other-keys)))
