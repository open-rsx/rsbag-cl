;;;; index.lisp --- Representation of TIDELog indices.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; Lazy timestamp sequence

#+sbcl
(defclass timestamps (standard-object
                      sequence)
  ((entries :initarg  :entries
            :type     index-vector
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
  (index-vector-length (timestamps-entries timestamps)))

#+sbcl
(defmethod sequence:elt ((timestamps timestamps)
                         (index      integer))
  (uint64->timestamp (index-vector-index->timestamp
                      index (timestamps-entries timestamps))))

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
       (let ((derived (when flush-strategy
                        (let ((class (find-class 'output-index)))
                          (c2mop:finalize-inheritance class)
                          (index-derive-flush-strategy
                           (c2mop:class-prototype class)
                          flush-strategy)))))
         (log:debug "~@<From flush strategy ~A, derived flush strategy ~
                     ~A~:@>"
                   flush-strategy derived)
         (apply #'make-it 'output-index
                (when derived (list :flush-strategy derived)))))
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
  (index-vector-length (index-entries index)))

(defmethod index-offset ((index  input-index)
                         (index1 integer))
  (index-vector-index->offset index1 (index-entries index)))

(defmethod index-offset ((index     input-index)
                         (timestamp local-time:timestamp))
  (index-vector-timestamp->offset timestamp (index-entries index)))

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
                                        :property :length/bytes
                                        :limit    (expt 2 22)))
  (:documentation
   "Instances of this class store partial index information for
    individual channels until it is written to the output stream.

    These indices do not generally store information for all events in
    one channel and cannot be queried."))

(defmethod index-derive-flush-strategy ((index          output-index)
                                        (flush-strategy t))
  flush-strategy)

(defmethod index-derive-flush-strategy
    ((index          output-index)
     (flush-strategy rsbag.backend::property-limit))
  (if (eq :length/bytes (rsbag.backend::flush-strategy-property flush-strategy))
      (let ((limit (rsbag.backend::flush-strategy-limit flush-strategy)))
        (make-flush-strategy :property-limit
                             :property :length/bytes
                             :limit    (floor limit 4)))
      flush-strategy))

(defmethod index-derive-flush-strategy
    ((index          output-index)
     (flush-strategy rsbag.backend::composite-flush-strategy-mixin))
  (apply #'make-flush-strategy
         (class-of flush-strategy)
         (mapcar (compose #'list (curry #'index-derive-flush-strategy index))
                 (rsbag.backend::flush-strategy-children flush-strategy))))

(defmethod index-num-entries ((index output-index))
  (indx-count (backend-buffer index)))  ; TODO wrong after flushing

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
            (when (>= timestamp sorted-to)
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
  (let+ (((&structure-r/o index- stream (sorted-to %sorted-to)) index)
         ((&structure-r/o indx- entries) buffer))
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
  ;; Next method updates current in-memory `indx' block.
  (call-next-method)

  ;; Add to entries.
  (index-vector-push-extend-entry (index-entries index) timestamp offset))
