;;; stream-mixin.lisp --- Mixin class for stream-based backends.
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

(defclass stream-mixin ()
  ((stream :initarg  :stream
	   :reader   backend-stream
	   :type     stream
	   :documentation
	   "Stores the stream which contains the data read and written
by the backend."))
  (:default-initargs
   :stream (required-argument :stream))
  (:documentation
   "This class is intended to be mixed into backend classes which
read/write data from/to a stream."))

(defmethod close ((backend stream-mixin)
		  &key &allow-other-keys)
  "Make sure the stream is closed."
  (when (next-method-p)
    (call-next-method))
  (close (backend-stream backend)))
