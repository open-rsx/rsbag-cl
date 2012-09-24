;;; backend-mixins.lisp --- Mixin classes for backend classes
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsbag.backend)


;;; `direction-mixin' mixin class
;;

(defclass direction-mixin ()
  ((direction :initarg  :direction
	      :type     rsbag:direction
	      :reader   backend-direction
	      :documentation
	      "Stores the direction with which the backend has been
opened."))
  (:default-initargs
   :direction (missing-required-initarg 'direction-mixin :direction))
  (:documentation
   "This class is intended to be mixed into backend classes that have
to keep track of the direction for which the data source has been
opened."))

(defmethod flush :around ((backend direction-mixin)
			  (buffer  t))
  (when (member (backend-direction backend) '(:output :io))
    (call-next-method)))


;;; `stream-mixin' mixin class
;;

(defclass stream-mixin (location-mixin)
  ((stream :initarg  :stream
	   :reader   backend-stream
	   :type     stream
	   :documentation
	   "Stores the stream which contains the data read and written
by the backend."))
  (:default-initargs
   :stream (missing-required-initarg 'stream-mixin :stream))
  (:documentation
   "This class is intended to be mixed into backend classes which
read/write data from/to a stream."))

(defmethod close ((backend stream-mixin)
		  &key &allow-other-keys)
  "Make sure the stream is closed."
  (when (next-method-p)
    (call-next-method))
  (close (backend-stream backend)))


;;; `location-mixin' mixin class
;;

(defclass location-mixin ()
  ((location :initarg  :location
	     :accessor backend-location
	     :initform nil
	     :documentation
	     "Stores the location to which the backend object is
connected. Can be NIL is such a location is not known."))
  (:documentation
   "This mixin allows remembering the location to which
a (e.g. stream-based) backend object is connected."))


;;; `buffering-writer-mixin' mixin class
;;

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
		  &key abort)
  "Flush the buffer if necessary, then proceed."
  (let+ (((&accessors-r/o (buffer backend-buffer)) backend))
    (when (and buffer (not abort))
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

(defmethod write-buffer :before ((backend t)
				 (buffer  t))
  (rsb:log1 :info backend "Writing ~A (~@[~:D entr~:@P~]~@[, ~:D ~
byte~:P~]~@[, ~,2F sec~:P~])"
	    buffer
	    (buffer-property backend buffer :length/entries)
	    (buffer-property backend buffer :length/bytes)
	    (buffer-property backend buffer :time-to-last-write)))


;;; `last-write-time-mixin' mixin class
;;

(defclass last-write-time-mixin ()
  ((last-write-time :initarg  :last-write-time
		    :type     (or null local-time:timestamp)
		    :accessor last-write-time
		    :initform nil
		    :documentation
		    "Stores the most recent time at which the
associated buffer has been flushed."))
  (:documentation
   "This class can be mixed into backend class which should expose the
most recent times at which their associated buffers have been
flushed."))

(defmethod buffer-property ((backend last-write-time-mixin)
			    (buffer  t)
			    (name    (eql :last-write-time)))
  (or (last-write-time backend)
      (setf (last-write-time backend) (local-time:now))))

(defmethod buffer-property ((backend last-write-time-mixin)
			    (buffer  t)
			    (name    (eql :time-to-last-write)))
  (when-let ((last-write-time (buffer-property
			       backend buffer :last-write-time)))
    (local-time:timestamp-difference (local-time:now) last-write-time)))

(defmethod flush :after ((backend last-write-time-mixin)
			 (buffer  t))
  (setf (last-write-time backend) (local-time:now)))
