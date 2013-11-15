;;;; backend-mixins.lisp --- Mixin classes for backend classes
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend)

;;; `direction-mixin' mixin class

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

(defclass stream-mixin (location-mixin)
  ((stream :initarg  :stream
           :reader   backend-stream
           :type     stream
           :documentation
           "Stores the stream which contains the data read and written
            by the backend.")
   (lock   :reader   backend-lock
           :initform (bt:make-lock "Stream lock")
           :documentation
           "Stores a lock protecting the stream against concurrent
            access."))
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

(defclass location-mixin ()
  ((location :initarg  :location
             :accessor backend-location
             :initform nil
             :documentation
             "Stores the location to which the backend object is
              connected. Can be NIL is such a location is not
              known."))
  (:documentation
   "This mixin allows remembering the location to which
    a (e.g. stream-based) backend object is connected."))

;;; `buffering-writer-mixin' mixin class

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
  (log:info "~@<~A is writing ~A (~@[~:D entr~:@P~]~@[, ~:D ~
             byte~:P~]~@[, ~,2F sec~:P~])~@:>"
            backend buffer
            (buffer-property backend buffer :length/entries)
            (buffer-property backend buffer :length/bytes)
            (buffer-property backend buffer :time-to-last-write)))

;;; `async-double-buffered-writer-mixin' mixin class

(declaim (special *async?*))

(defvar *async?* t
  "Indicates whether an async operation should be performed and
   especially to disallow async operations under certain conditions.")

(defclass async-double-buffered-writer-mixin ()
  ((back-buffer   :accessor back-buffer
                  :initform nil
                  :documentation
                  "Stores a buffer which can be used by the backend
                   while an associated buffer is being written
                   back. The value changes when the buffers swap
                   roles.")
   (writer        :accessor %writer
                  :initform nil
                  :documentation
                  "When non-nil, stores a future object which
                   eventually return the result of async write-back
                   operation."))
  (:documentation
   "This class transparently adds to buffer-based backend classes the
    ability to write buffers asynchronously to other operations
    performed by the backend."))

(defmethod shared-initialize :after ((instance   async-double-buffered-writer-mixin)
                                     (slot-names t)
                                     &key)
  ;; Ask the backend to provide an additional buffer for async
  ;; operations.
  (setf (back-buffer instance) (make-buffer instance nil)))

(defmethod close :around ((backend async-double-buffered-writer-mixin)
                          &key abort)
  (declare (ignore abort))

  (with-threadpool
    ;; Force any unfinished write operation to finish.
    (when (%writer backend)
      (lparallel:force (%writer backend)))

    ;; Continue with the default closing behavior but disallow async
    ;; operation.
    (let ((*async?* nil))
      (call-next-method))))

(defmethod write-buffer :around ((backend async-double-buffered-writer-mixin)
                                 (buffer  t))
  (with-threadpool
    ;; When async operation is disallowed, just call the next method.
    (unless *async?*
      (return-from write-buffer (call-next-method)))

    ;; Force any unfinished write operation to finish and retrieve the
    ;; buffer it used.
    (when (%writer backend)
      (setf (back-buffer backend) (lparallel:force (%writer backend))))

    ;; Start a new async write operation on BUFFER. The result will be
    ;; collected by the next write or close operation.
    (setf (%writer backend)
          (lparallel:future
            (let ((*async?* nil))
              (log:info "~@<~A is starting to flush buffer ~A~@:>" backend buffer)
              (write-buffer backend buffer)
              (log:info "~@<~A has finished flushing buffer ~A~@:>" backend buffer)
              buffer)))))

(defmethod make-buffer :around ((backend async-double-buffered-writer-mixin)
                                (buffer  t))
  ;; Ask for more buffers until we have a "front" and a "back" buffer.
  (if (or (null buffer) (null (back-buffer backend)))
      (call-next-method)
      (call-next-method backend (back-buffer backend))))

;;; `last-write-time-mixin' mixin class

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
