;;; mock-backend.lisp --- Mock backend class for unit tests.
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

(defclass mock-backend ()
  ((channels :initarg  :channels
	     :type     list
	     :accessor %backend-channels
             :initform nil
	     :documentation
	     "List of mock-channel data of the form

  (TIMESTAMPS ENTRIES ID NAME META-DATA)

where

TIMESTAMPS is a list of `local-time:timestamp' objects,

ENTRIES is a list of raw entries,

ID is the integer id of the channel,

NAME is a string naming the channel,

and META-DATA is a plist containing the meta-data for the channel.")))

(defmethod close ((stream mock-backend) &key abort)
  (declare (ignore abort)))

(defmethod rsbag.backend:backend-location ((backend mock-backend))
  nil)

(defmethod rsbag.backend:get-channels ((backend mock-backend))
  (mapcar (curry #'nthcdr 2) (%backend-channels backend)))

(defmethod rsbag.backend:make-channel-id ((backend mock-backend)
					  (channel t))
  (length (%backend-channels backend)))

(defmethod rsbag.backend:put-channel ((backend   mock-backend)
				      (channel   integer)
				      (name      string)
				      (meta-data list))
  (appendf (%backend-channels backend)
	   (list (list nil nil channel name meta-data))))

(defmethod rsbag.backend:get-num-entries ((backend mock-backend)
					  (channel integer))
  (length (rsbag.backend:get-timestamps backend channel)))

(defmethod rsbag.backend:get-timestamps ((backend mock-backend)
					 (channel integer))
  (first (nth channel (%backend-channels backend))))

(defmethod rsbag.backend:get-entry ((backend mock-backend)
				    (channel integer)
				    (index   integer))
  (nth index (second (nth channel (%backend-channels backend)))))

(defmethod rsbag.backend:put-entry ((backend mock-backend)
				    (channel integer)
				    (index   local-time:timestamp)
				    (entry   t))
  (let ((channel (nth channel (%backend-channels backend))))
    (appendf (first channel) (list index))
    (appendf (second channel) (list entry))))

;;; Convenience macros

(defmacro with-mock-backend ((backend-var) (&body content) &body body)
  "Execute BODY with BACKEND-VAR bound to a `mock-backend' instance
filled with CONTENT."
  `(let ((,backend-var (make-instance 'mock-backend
				      :channels (list ,@content))))
     ,@body))

(defmacro with-mock-bag ((bag-var &rest initargs) (&body content) &body body)
  "Execute BODY with BAG-VAR bound to a `bag' instance backed by a
`mock-backend' instance filled with CONTENT. INITARGS are passed to
the constructed `bag' instance."
  (with-gensyms (backend-var)
    `(with-mock-backend (,backend-var) (,@content)
       (let ((,bag-var (make-instance 'bag
				      :backend ,backend-var
				      ,@initargs)))
	 ,@body))))

(defun simple-bag ()
  (with-mock-bag (bag :direction :input)
      (`((,(local-time:now) ,(local-time:now))
	 (1 2)
	 0 "/foo"
	 ())
       `((,(local-time:now) ,(local-time:now) ,(local-time:now))
	 (3 4 5)
	 1 "/bar"
	 ()))
    bag))
