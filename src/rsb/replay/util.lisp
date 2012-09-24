;;; util.lisp ---
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

(defclass informer-injector (channel-items)
  ((informer :initarg  :informer
	     :reader   %informer-injector-informer
	     :documentation
	     "Stores the informer that should be associated with the
channel."))
  (:documentation
   "Instance of this helper class inject a given object (usually an
`rsb:informer' instance) into each element of the underlying
sequence."))

(defmethod sequence:elt ((sequence informer-injector)
			 (index    integer))
  (append (call-next-method)
	  (list (%informer-injector-informer sequence))))

(defun inject-informer (channel connection)
  ;; Find the channel-connection for CHANNEL in CONNECTION, extract
  ;; the informer and pass it to a new `informer-injector' instance.
  (make-instance 'informer-injector
		 :channel  channel
		 :informer (connection-endpoint
			    (find channel (connection-channels connection)
				  :test #'member
				  :key  #'connection-channels))))


;;; Utility functions
;;

(defun %make-progress-reporter (sequence callback)
  "Return a function with two parameters that calls CALLBACK in the
appropriate way if CALLBACK is non-nil"
  (when callback
    (let+ (((start end) (list 0 (1- (length sequence)))))
      #'(lambda (index timestamp)
	  (funcall callback
		   (/ (1+ (- index start)) (- (1+ end) start))
		   index start end
		   timestamp)))))
