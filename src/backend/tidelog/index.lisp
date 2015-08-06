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

;;;

(defclass index (direction-mixin
                 async-double-buffered-writer-mixin
                 buffering-writer-mixin
                 last-write-time-mixin)
  ((channel   :initarg  :channel
              :type     non-negative-integer
              :reader   index-channel
              :documentation
              "Stores the id of the channel to which this index
               belongs.")
   (entries   :type     index-vector
              :accessor index-entries
              :initform (make-index-vector)
              :documentation
              "Stores the actual timestamp -> offset mapping. The
               storage is sorted and interleaved of the form

                 TIMESTAMP1 OFFSET1 TIMESTAMP2 OFFSET2 ...

               .")
   (sorted-to :initarg  :sorted-to
              :type     (or integer null)
              :accessor index-%sorted-to
              :initform 0
              :documentation
              "Stores the index into the entries vector up to which
               entries are sorted. The value nil indicates that
               entries are not sorted.")
   (stream    :initarg  :stream
              :type     stream
              :reader   index-stream
              :documentation
              "Stores the stream to which the data of this index
               should be written when flushing.")
   (lock      :initarg  :lock
              :reader   index-%lock
              :documentation
              "Stores a lock that protects the stream."))
  (:default-initargs
   :channel        (missing-required-initarg 'index :channel)
   :stream         (missing-required-initarg 'index :stream)
   :lock           (missing-required-initarg 'index :lock)
   :flush-strategy (make-flush-strategy :property-limit
                                        :property :length/entries
                                        :limit    most-positive-fixnum))
  (:documentation
   "Instances of this class store mappings of indices and timestamps
    of entries to corresponding file offsets for one channel."))

(defmethod index-add-indxs ((index  index)
                            (indxs  sequence)
                            (chunks vector))
  (index-vector-add-indxs (index-entries index) indxs chunks))

(defmethod index-add-entries ((index   index)
                              (entries sequence)
                              (chunks  vector))
  (index-vector-add-entries (index-entries index) entries chunks))

(defmethod index-num-entries ((index index))
  (/ (length (index-entries index)) 2))

(defmethod index-offset ((index index)
                         (index1 integer))
  (aref (index-entries index) (1+ (* 2 index1))))

(defmethod index-offset ((index     index)
                         (timestamp local-time:timestamp))
  (%timestamp->index timestamp (index-entries index)))

(defmethod put-entry ((index     index)
                      (timestamp integer)
                      (offset    integer)
                      (chunk-id  integer))
  (let+ (((&accessors-r/o (buffer    backend-buffer)
                          (entries   index-entries)
                          (sorted-to index-%sorted-to))
          index))
    ;; Add to entries.
    (index-vector-push-extend-entry entries timestamp offset)

    ;; Update index block.
    (incf (indx-count buffer))
    (vector-push-extend
     (make-instance 'index-entry
                    :chunk-id  chunk-id
                    :timestamp timestamp
                    :offset    offset)
     (indx-entries buffer))

    ;; Update sorted state.
    (when sorted-to
      (setf (index-%sorted-to index)
            (when (> timestamp sorted-to)
              timestamp)))))

;;; Buffering

(defmethod make-buffer ((index  index)
                        (buffer (eql nil)))
  (make-buffer index (make-instance 'indx
                                    :channel-id (index-channel index))))

(defmethod make-buffer ((index  index)
                        (buffer indx))
  (reinitialize-instance buffer
                         :count   0
                         :entries (make-array 0
                                              :adjustable   t
                                              :fill-pointer 0)))

(defmethod write-buffer ((index  index)
                         (buffer indx))
  ;; If some timestamps have been inserted out of order, sort the
  ;; entire index block now.
  (unless (index-%sorted-to index)
    (warn "~@<Sorting index block due to out-of-order ~
           insertions.~@:>")
    (let ((entries (indx-entries buffer)))
      (sort entries #'< :key #'index-entry-timestamp)
      (setf (indx-entries buffer) entries)))

  ;; If we have anything to write, write it and reset fill pointers so
  ;; we can start filling the buffer again.
  (unless (zerop (indx-count buffer))
    (bt:with-lock-held ((index-%lock index))
      (pack buffer (index-stream index))
      (force-output (index-stream index)))))

(defmethod buffer-property ((backend index)
                            (buffer  indx)
                            (name    (eql :length/entries)))
  (indx-count buffer))

(defmethod buffer-property ((backend index)
                            (buffer  indx)
                            (name    (eql :length/bytes)))
  (+ 8 (* 20 (buffer-property backend buffer :length/entries))))
