;;; mixins.lisp --- Unit tests for backend mixin classes.
;;
;; Copyright (C) 2013 Jan Moringen
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

(cl:in-package :rsbag.backend.test)


;;; `mock-buffering-backend' class
;;

(define-condition mock-write-back-error (error)
  ())

(defclass mock-buffering-backend ()
  ((written :initarg  :written
	    :type     list
	    :accessor backend-written
	    :initform nil)))

(defmethod make-buffer ((backend mock-buffering-backend)
			(buffer  t))
  (list (if buffer (first buffer) (gensym)) nil))

(defmethod write-buffer ((backend mock-buffering-backend)
			 (buffer  t))
  ;; Sleep a little while to simulate slow write-back. This is helpful
  ;; for tests which try to detect failure to write-back data.
  (sleep .001)
  (case (second buffer)
    (error (error 'mock-write-back-error))
    (t     (appendf (backend-written backend) (list buffer))))
  (values))

(defmethod close ((backend mock-buffering-backend)
		  &key abort)
  (declare (ignore abort)))


;;;
;;

(defclass async-double-buffered-writer-mock-backend (mock-buffering-backend
						     async-double-buffered-writer-mixin)
  ())

(deftestsuite rsbag.backend.async-double-buffered-writer (backend-root)
  ()
  (:documentation
   "Unit tests for the `async-double-buffered-writer' class."))

(addtest (rsbag.backend.async-double-buffered-writer
          :documentation
	  "Make sure that that the async write-back of
`async-double-buffered-writer' write everything and in the correct
order.")
  write-back/smoke

  (ensure-cases (data)
      `(()
	(1)
	,(iota 10)
	,(iota 100))

    (let+ ((backend        (make-instance 'async-double-buffered-writer-mock-backend))
	   (expected       '())
	   (current-buffer nil)
	   ((&flet do-one (data)
	      (setf current-buffer (make-buffer backend current-buffer))
	      (setf (second current-buffer) data)
	      (appendf expected (list current-buffer))
	      (write-buffer backend current-buffer))))
      (mapcar #'do-one data)
      (close backend)

      (ensure-same (backend-written backend) expected :test #'equal))))

(addtest (rsbag.backend.async-double-buffered-writer
          :documentation
	  "Make sure that the next operation after a failed async
write-back of `async-double-buffered-writer' operation signals the
error.")
  write-back/conditions

  (macrolet
      ((with-write-back-error (&body body)
	 `(let* ((backend (make-instance 'async-double-buffered-writer-mock-backend))
		 (buffer  (make-buffer backend nil)))
	    (setf (second buffer) 'error) ; signals an error on write-back
	    (write-buffer backend buffer) ; does not signal immediately

	    ;; We expect the next operation, BODY, to receive the
	    ;; error.
	    (ensure-condition 'mock-write-back-error ,@body))))

    ;; Catch the async error in the next write operation.
    (with-write-back-error
      (setf buffer          (make-buffer backend buffer)
	    (second buffer) 1)
      (write-buffer backend buffer))

    ;; Catch the async error when closing the backend.
    (with-write-back-error
      (close backend))))
