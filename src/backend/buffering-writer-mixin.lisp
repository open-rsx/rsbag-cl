;;; buffering-writer-mixin.lisp ---
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

(in-package :rsbag.backend)

(defclass buffering-writer-mixin ()
  ((buffer      :accessor backend-buffer
		:initform nil
		:documentation
		"")
   (flush?-func :initarg  :flush?-func
		:type     function
		:accessor backend-flush?-func
		:documentation
		""))
  (:default-initargs
   :flush?-func (required-argument :flush?-func))
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
  (bind (((:accessors-r/o (buffer backend-buffer)) backend))
    (when buffer
      (write-buffer backend buffer))
    (when (next-method-p)
      (call-next-method))))

(defmethod put-entry :after ((backend buffering-writer-mixin)
			     (channel t)
			     (index   t)
			     (entry   t))
  "After adding an entry, check whether the buffer has to be flushed
and potentially do it."
  (bind (((:accessors-r/o (buffer      backend-buffer)
			  (flush?-func backend-flush?-func)) backend))
    (when (funcall flush?-func backend buffer)
      (write-buffer backend buffer))))

(defmethod write-buffer :after ((backend buffering-writer-mixin)
				(buffer  t))
  "Reset the buffer after flushing."
  (setf (backend-buffer backend) (make-buffer backend buffer)))
