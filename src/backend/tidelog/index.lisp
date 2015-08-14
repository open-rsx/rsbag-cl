;;;; index.lisp --- Representation of TIDELog indices.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; Index vector

(deftype index-vector ()
  '(array (unsigned-byte 64) (*)))

(defun make-index-vector ()
  (make-array 0 :element-type '(unsigned-byte 64)
                :adjustable   t
                :fill-pointer 0))

(declaim (inline index-vector-push-entry
                 index-vector-push-extend-entry))
(defun index-vector-push-entry (index timestamp offset)
  (vector-push timestamp index)
  (vector-push offset    index))

(defun index-vector-push-extend-entry (index timestamp offset)
  (vector-push-extend timestamp index)
  (vector-push-extend offset    index))

(defun index-vector-add-entries (index entries chunks)
  (declare (type index-vector index))
  (let+ ((num-entries (length entries))
         ((&flet add-offset! (entry)
            (let+ (((&structure-r/o index-entry- timestamp chunk-id offset)
                    entry)
                   (outer-offset  (%chunk-id->offset chunk-id chunks))
                   (global-offset (+ outer-offset 12 25 offset))) ; TODO(jmoringe):  get rid of the constants
              (index-vector-push-entry index timestamp global-offset)))))
    (adjust-array index (+ (length index) (* 2 num-entries)))
    (map nil #'add-offset! entries)
    index))

(defun index-vector-add-indxs (index indxs chunks)
  (declare (type index-vector index))
  (let ((num-entries (reduce #'+ indxs :key #'indx-count)))
    (adjust-array index (+ (length index) (* 2 num-entries)))
    (iter (for indx in-sequence indxs)
          (index-vector-add-entries index (indx-entries indx) chunks))
    index))

(declaim (ftype (function ((unsigned-byte 64) index-vector
                           &optional
                           non-negative-fixnum
                           non-negative-fixnum)
                          (unsigned-byte 64))
                %timestamp->index))

(defun %timestamp->index (timestamp index
                          &optional
                          (start 0)
                          (end   (length index)))
  (let* ((pivot  (ash (+ start end) -1))
         (pivot* (aref index (* 2 pivot))))
    (cond
      ((< pivot* timestamp)
       (%timestamp->index timestamp index pivot end))
      ((> pivot* timestamp)
       (%timestamp->index timestamp index start pivot))
      (t (aref index (1+ (* 2 pivot)))))))

;;; Lazy timestamp sequence

#+sbcl
(defclass timestamps (standard-object
                      sequence)
  ((entries :initarg  :entries
            :type     vector
            :reader   timestamps-entries
            :documentation
            "Points to the list of entries of the associated index."))
  (:default-initargs
   :entries (missing-required-initarg 'timestamps :entries))
  (:documentation
   "Instances of this class are sequence of `local-time:timestamp'
    instances that produced lazily."))

#+sbcl
(defmethod sequence:length ((timestamps timestamps))
  (ash (length (timestamps-entries timestamps)) -1))

#+sbcl
(defmethod sequence:elt ((timestamps timestamps)
                         (index      integer))
  (uint64->timestamp
   (aref (timestamps-entries timestamps) (* 2 index))))

;;; Index creation

(defun make-index (channel-id stream lock direction
                   &key flush-strategy)
  (let+ (((&flet make-it (class &rest initargs)
            (apply #'make-instance class
                   :stream    stream
                   :lock      lock
                   :direction direction
                   :channel   channel-id
                   initargs))))
    (ecase direction
      (:input
       (make-it 'input-index))
      (:output
       (apply #'make-it 'output-index
              (when flush-strategy
                (list :flush-strategy flush-strategy))))
      (:io
       (make-it 'io-index)))))

;;; `base-index'

(defclass base-index (direction-mixin)
  ((channel :initarg  :channel
            :type     non-negative-integer
            :reader   index-channel
            :documentation
            "Stores the id of the channel to which this index
             belongs.")
   (stream  :initarg  :stream
            :type     stream
            :reader   index-stream
            :documentation
            "Stores the stream to which the data of this index should
             be written when flushing.")
   (lock    :initarg  :lock
            :reader   index-%lock
            :documentation
            "Stores a lock that protects the stream."))
  (:default-initargs
   :channel (missing-required-initarg 'base-index :channel)
   :stream  (missing-required-initarg 'base-index :stream)
   :lock    (missing-required-initarg 'base-index :lock))
  (:documentation
   "Superclass of index classes."))

;;; `input-index'

(defclass input-index (base-index)
  ((entries :type     index-vector
            :accessor index-entries
            :initform (make-index-vector)
            :documentation
            "Stores the actual timestamp -> offset mapping. The
             storage is sorted and interleaved of the form

               TIMESTAMP1 OFFSET1 TIMESTAMP2 OFFSET2 ...

             ."))
  (:documentation
   "Instances of this class index events of individual channels for
    files opened with :input direction.

    These indices only store event timestamps and corresponding
    offsets, are immutable and never write back any data."))

(defmethod close ((stream input-index) &key abort)
  (declare (ignore abort))) ; nothing to do

(defmethod index-num-entries ((index input-index))
  (/ (length (index-entries index)) 2))

(defmethod index-offset ((index  input-index)
                         (index1 integer))
  (aref (index-entries index) (1+ (* 2 index1))))

(defmethod index-offset ((index     input-index)
                         (timestamp local-time:timestamp))
  (%timestamp->index timestamp (index-entries index)))

(defmethod index-add-indxs ((index  input-index)
                            (indxs  sequence)
                            (chunks vector))
  (index-vector-add-indxs (index-entries index) indxs chunks))

(defmethod index-add-entries ((index   input-index)
                              (entries sequence)
                              (chunks  vector))
  (index-vector-add-entries (index-entries index) entries chunks))

;;; `output-index'

(defclass output-index (base-index
                        async-double-buffered-writer-mixin
                        buffering-writer-mixin
                        last-write-time-mixin)
  ((sorted-to :initarg  :sorted-to
              :type     (or integer null)
              :accessor index-%sorted-to
              :initform 0
              :documentation
              "Stores the index into the entries vector up to which
               entries are sorted. The value nil indicates that
               entries are not sorted."))
  (:default-initargs
   :flush-strategy (make-flush-strategy :property-limit
                                        :property :length/entries
                                        :limit    most-positive-fixnum))
  (:documentation
   "Instances of this class store partial index information for
    individual channels until it is written to the output stream.

    These indices do not generally store information for all events in
    one channel and cannot be queried."))

(defmethod initialize-instance :after ((instance output-index) &key)
  (setf (indx-channel-id (backend-buffer instance))
        (index-channel instance)))

(defmethod index-num-entries ((index output-index))
  (indx-count (backend-buffer index))) ; TODO wrong after flushing

(defmethod put-entry ((index     output-index)
                      (timestamp integer)
                      (offset    integer)
                      (chunk-id  integer))
  (let+ (((&accessors-r/o (buffer    backend-buffer)
                          (sorted-to index-%sorted-to))
          index))
    ;; Update index block.
    (incf (indx-count buffer))
    (make-or-reuse-instance
     (indx-entries buffer) index-entry
     :chunk-id  chunk-id
     :timestamp timestamp
     :offset    offset)

    ;; Update sorted state.
    (when sorted-to
      (setf (index-%sorted-to index)
            (when (> timestamp sorted-to)
              timestamp)))))

;; Buffering

(defmethod make-buffer ((index    output-index)
                        (previous (eql nil)))
  (make-buffer index (make-instance 'indx
                                    :channel-id (index-channel index)
                                    :entries    (make-array 0
                                                            :adjustable   t
                                                            :fill-pointer 0))))

(defmethod make-buffer ((index    output-index)
                        (previous indx))
  (setf (fill-pointer (indx-entries previous)) 0)
  (reinitialize-instance previous :count 0))

(defmethod write-buffer ((index  output-index)
                         (buffer indx))
  (let+ (((&accessors-r/o (stream    index-stream)
                          (sorted-to index-%sorted-to))
          index)
         ((&accessors-r/o (entries indx-entries)) buffer))
    ;; If some timestamps have been inserted out of order, sort the
    ;; entire index block now.
    (unless sorted-to
      (warn "~@<Sorting index block due to out-of-order ~
             insertions.~@:>")
      (setf entries (sort entries #'< :key #'index-entry-timestamp)))

    ;; If we have anything to write, write it and reset fill pointer
    ;; so we can start filling the buffer again.
    (unless (zerop (indx-count buffer))
      (bt:with-lock-held ((index-%lock index))
        (pack buffer stream)
        (force-output stream)))))

(defmethod buffer-property ((backend output-index)
                            (buffer  indx)
                            (name    (eql :length/entries)))
  (indx-count buffer))

(defmethod buffer-property ((backend output-index)
                            (buffer  indx)
                            (name    (eql :length/bytes)))
  (+ 8 (* 20 (buffer-property backend buffer :length/entries))))

;;; `io-index'

(defclass io-index (input-index
                    output-index)
  ()
  (:documentation
   "Instances of these class manage mutable and queryable index
    information for individual channels.

    Such an index contains timestamp and index information for all
    events of the associated channel as well as a buffer of unwritten
    index information."))

(defmethod put-entry ((index     io-index)
                      (timestamp integer)
                      (offset    integer)
                      (chunk-id  integer))
  ;; Next methods updates current in-memory indx block.
  (call-next-method)

  ;; Add to entries.
  (index-vector-push-extend-entry (index-entries index) timestamp offset))
