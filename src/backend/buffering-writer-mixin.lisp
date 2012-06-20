;;; buffering-writer-mixin.lisp ---
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

(cl:in-package :rsbag.backend)

(defclass buffering-writer-mixin ()
  ((buffer         :accessor backend-buffer
		   :initform nil
		   :documentation
		   "Stores a buffer which is flushed when `flush?' is
non-nil.")
   (flush-strategy :initarg  :flush-strategy
		   :accessor backend-flush-strategy
		   :documentation
		   "Stores a strategy that is used to determine
whether the current buffer should be flushed."))
  (:default-initargs
   :flush-strategy (missing-required-initarg
		    'buffering-writer-mixin :flush-strategy))
  (:documentation
   "This class is intended to be mixed into backend classes that
buffer added entries before writing them to disk."))

(defmethod shared-initialize :after ((instance   buffering-writer-mixin)
                                     (slot-names t)
                                     &key)
  (setf (backend-buffer instance) (make-buffer instance nil)))

(defmethod close ((backend buffering-writer-mixin)
		  &key &allow-other-keys)
  "Flush the buffer if necessary, then proceed."
  (let+ (((&accessors-r/o (buffer backend-buffer)) backend))
    (when buffer
      (flush backend buffer))
    (when (next-method-p)
      (call-next-method))))

(defmethod put-entry :after ((backend buffering-writer-mixin)
			     (channel t)
			     (index   t)
			     (entry   t))
  "After adding an entry, check whether the buffer has to be flushed
and potentially do it."
  (let+ (((&accessors-r/o (buffer   backend-buffer)
			  (strategy backend-flush-strategy)) backend))
    (when (flush? strategy backend buffer)
      (flush backend buffer))))

(defmethod flush ((backend buffering-writer-mixin)
		  (buffer  t))
  (write-buffer backend buffer))

(defmethod flush :after ((backend buffering-writer-mixin)
			 (buffer  t))
  "Reset the buffer of BACKEND after flushing."
  (setf (backend-buffer backend) (make-buffer backend buffer)))
