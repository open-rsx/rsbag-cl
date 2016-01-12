;;;; file.lisp --- The file class represents a TIDE log file.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

(defclass file (stream-mixin
                direction-mixin
                async-double-buffered-writer-mixin
                buffering-writer-mixin
                last-write-time-mixin)
  ((channels        :type     hash-table
                    :accessor file-%channels
                    :initform (make-hash-table :test #'eql)
                    :documentation
                    "Stores information of the channels present in the
                     file. Entries are of the form

                       (ID NAME META-DATA)

                     where ID is the integer id of the channel, NAME
                     is string name of the channel, META-DATA is a
                     plist containing the well-known
                     entries :type, :format, :source-name,
                     :source-config and potentially others.")
   #+no (indices         :type     hash-table
                    :reader   file-%indices
                    :initform (make-hash-table :test #'eq)
                    :documentation
                    "Maps channel ids to index objects.")
   (next-channel-id :type     non-negative-integer
                    :accessor file-%next-channel-id
                    :initform 0
                    :documentation
                    "Stores the id that will be assigned to the next
                     new channel.")
   (next-chunk-id   :type     non-negative-integer
                    :accessor file-%next-chunk-id
                    :initform 0
                    :documentation
                    "Stores the id that will be assigned to the next
                     new chunk."))
  (:default-initargs
   :flush-strategy (make-flush-strategy :property-limit
                                        :property :length/bytes
                                        :limit    (expt 2 25)))
  (:documentation
   "Instances of this class represent files using the TIDE log file
    format as specified at https://retf.info/svn/drafts/rd-0001.txt."))

(service-provider:register-provider/class
 'backend :tide :class 'file)

(defmethod shared-initialize :after ((instance   file)
                                     (slot-names t)
                                     &key)
  ;; If the file is completely empty, add a TIDE header.
  (let+ (((&structure-r/o backend- stream direction) instance))
    (case direction
      (:output
       (maybe-write-header stream))
      (:io
       (maybe-write-header stream)
       (file-position stream 0))))

  ;; Scan through the TIDElog file collecting deserialized CHAN and
  ;; INDX blocks and the *ids and file offsets* of CHNK blocks.
  ;; Use the INDX blocks to build per-channel indices.
  (let+ (((&accessors (stream          backend-stream)
                      (lock            rsbag.backend::backend-lock)
                      (direction       backend-direction)
                      (buffer          backend-buffer)
                      (flush-strategy  backend-flush-strategy)
                      (channels        file-%channels)
                      #+no (indices         file-%indices)
                      (next-channel-id file-%next-channel-id)
                      (next-chunk-id   file-%next-chunk-id))
          instance)
         (indices (make-hash-table :test #'eql))
         ((&values chans indxs chnks complete?)
          (when (member direction '(:input :io))
            (scan stream :tide)))
         ((&flet ensure-index-channel (index)
            (let ((id (index-channel index)))
              (ensure-gethash
               id channels
               (restart-case
                   (progn
                     (setf complete? nil)
                     (error "~@<No channel with id ~D.~@:>" id))
                 (continue (&optional condition)
                   :report (lambda (stream)
                             (format stream "~@<Reconstruct channel ~D.~@:>"
                                     id))
                   (declare (ignore condition))
                   (log:info "~@<Reconstructing channel ~D~@:>" id)
                   (let ((name (format nil "reconstructed~D" id)))
                     (make-channel (list :id        id
                                         :name      name
                                         :meta-data '(:type :bytes)
                                         :index     index))))
                 (use-value (name meta-data)
                   :report (lambda (stream)
                             (format stream "~@<Use the supplied ~
                                             description for channel ~
                                             ~D.~@:>"
                                     id))
                   (make-channel (list :id        id
                                       :name      name
                                       :meta-data meta-data
                                       :index     index))))))))
         ((&flet make-index (channel-id)
            (let ((lock (rsbag.backend::backend-lock instance)))
              (setf (gethash channel-id indices)
                    (make-index channel-id stream lock direction
                                :flush-strategy flush-strategy)))))
         #+unused
         ((&flet check-index (channel-id)
            (or (gethash channel-id indices)
                (restart-case
                    (progn
                      (setf complete? nil)
                      (error "~@<No index for channel id ~D.~@:>"
                             channel-id))
                  (continue (&optional condition)
                    :report (lambda (stream)
                              (format stream "~@<Reconstruct index for ~
                                              channel ~D.~@:>"
                                      channel-id))
                    (declare (ignore condition))
                    (log:info "~@<Reconstructing index for channel ~D.~@:>"
                              channel-id)
                    (make-index channel-id))))))
         ((&flet ensure-index (channel-id)
            (or (gethash channel-id indices)
                (make-index channel-id))))
         ((&flet ensure-channel-index (channel)
            (unless (channel-index channel)
              (setf (channel-index channel)
                    (ensure-index (channel-id channel)))))))

    ;; Sort chunks for faster lookup during index building step.
    (setf chnks                  (sort (coerce chnks 'vector) #'<
                                       :key #'car)
          next-chunk-id          (1+ (reduce #'max chnks
                                             :initial-value -1
                                             :key           #'car))
          next-channel-id        (1+ (reduce #'max chans
                                             :initial-value -1
                                             :key           #'chan-id))
          (chnk-chunk-id buffer) next-chunk-id)
    (map nil (lambda (chan)
               (let+ (((&values id channel) (make-channel chan)))
                 (setf (gethash id channels) channel)))
         chans)


    ;; Construct `index' instances and, when necessary, missing
    ;; channels.
    (when (and (not (emptyp chnks)) (emptyp indxs))
      (setf complete? nil))
    (iter (with reconstructed? = nil)
          (restart-case
              (progn
                ;; Unpack INDX blocks into `index' instances. Multiple
                ;; INDX blocks can be merged into a single `index'
                ;; instance.
                ;;
                ;; If this signals an error, the CONTINUE restart can
                ;; be used to regenerate indices.
                (iter (for (channel-id . offset) in indxs)
                      (let ((indx  (unpack stream :block offset))
                            (index (ensure-index channel-id)))
                        (index-add-indxs index (list indx) chnks)))

                ;; Make sure that entries in CHANNELS exist for all
                ;; channel ids mentioned in the constructed
                ;; indices. This problem can occur, when INDX blocks
                ;; are written, but CHAN blocks are missing.
                ;;
                ;; If this signals an error, the CONTINUE restart can
                ;; be used to regenerate indices.
                (mapc #'ensure-index-channel (hash-table-values indices))

                ;; Make sure that `index' instances exist for all
                ;; channels in CHANNELS. This can problem can occur
                ;; when CHAN blocks are written, but INDX blocks are
                ;; missing.
                ;;
                ;; If this fails, the CONTINUE restart can be used to
                ;; regenerate indices.
                #+no (mapc (compose #'check-index #'first) channels)
                ;; INDX chunks may currently be omitted for empty
                ;; channels.
                #+no (mapc #'ensure-index (hash-table-keys channels))
                (mapc #'ensure-channel-index (hash-table-values channels))

                ;; If nothing above signaled an error but we still
                ;; know that the data is incomplete, signal an error
                ;; here.
                ;;
                ;; The CONTINUE restart can be used to regenerate
                ;; indices.
                (unless (or (eq direction :output) complete? reconstructed?)
                  (error "~@<Incomplete indices; read ~
                          ~:D chunk~:P (CHNK blocks), ~
                          ~:D channel~:P (CHAN blocks) and ~
                          ~:D ~:*~[indices~;index~:;indices~] (INDX blocks).~@:>"
                         (length chnks) (length chans) (length indxs)))

                (return))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Scan through all chunks and ~
                                        regenerate indices.~@:>"))
              :test   (lambda (condition) ; avoid infinite retries
                        (declare (ignore condition))
                        (not reconstructed?))
              (declare (ignore condition))
              (reconstruct-indices stream chnks #'ensure-index)
              (setf reconstructed? t    ; detect infinite retries
                    complete?      t
                    indxs          '()))
            (use-value (new-indxs)
              :report (lambda (stream)
                        (format stream "~@<Use the supplied value as ~
                                        list of indices and ~
                                        continue.~@:>"))
              (setf complete? t
                    indxs     new-indxs))))))

(defmethod close ((file file) &key abort)
  (map nil (rcurry #'close :abort abort)
       (mapcar #'channel-index (hash-table-values (file-%channels file)))) ; TODO make a function
  (when (next-method-p)
    (call-next-method)))

(defmethod make-channel-id ((file file) (name string))
  (prog1
      (file-%next-channel-id file)
    (incf (file-%next-channel-id file))))

(defmethod get-channels ((file file))
  (let ((result '()))
    (maphash (lambda (key channel)
               (declare (ignore key))
               (let+ (((&structure-r/o channel- id name meta-data data-length)
                       channel))
                 (push (list id name (list* :length data-length  meta-data))
                       result)))
             (file-%channels file))
    result))

(defmethod put-channel ((file      file)
                        (channel   integer)
                        (name      string)
                        (meta-data list))
  (let+ (((&accessors (stream         backend-stream)
                      (lock           rsbag.backend::backend-lock)
                      (direction      backend-direction)
                      (flush-strategy backend-flush-strategy)
                      (channels       file-%channels)
                      #+no (indices        file-%indices))
          file))
    ;; Create and store channel object.
    (let* ((index    (make-index channel stream lock direction
                                 :flush-strategy flush-strategy))
           (channel1 (make-instance 'channel
                                    :id          channel
                                    :name        name
                                    :meta-data   meta-data
                                    :data-length 0
                                    :index       index)))
      (setf (gethash channel channels) channel1))
    ;; Create an write CHAN block.
    (let+ (((&plist-r/o (type          :type)
                        (source-name   :source-name   "")
                        (source-config :source-config "")
                        (format        :format        ""))
            meta-data)
           (channel (make-instance 'chan
                                   :id            channel
                                   :name          name
                                   :type          (encode-type type)
                                   :source-name   source-name
                                   :source-config source-config
                                   :format        format)))
      (bt:with-lock-held (lock)
        (pack channel stream)))))

(defmethod get-num-entries ((file    file)
                            (channel integer))
  (let ((index (channel-index (gethash channel (file-%channels file))))) ; TODO make a function?
    (index-num-entries index)))

(defmethod get-timestamps ((file    file)
                           (channel integer))
  (let ((index (channel-index (gethash channel (file-%channels file)))))
    #+sbcl
    (make-instance 'timestamps :entries (index-entries index))
    #-sbcl
    #.(error "Not implemented.")))

(defmethod put-entry ((file      file)
                      (channel   integer)
                      (timestamp local-time:timestamp)
                      (entry     simple-array))
  (let+ (((&accessors-r/o (buffer   backend-buffer)
                          (channels file-%channels))
          file)
         (channel1   (gethash channel channels))
         (index      (channel-index channel1))
         (timestamp* (timestamp->uint64 timestamp))
         (length     (length entry)))

    ;; Note increased amount of data in channel.
    (incf (channel-data-length channel1) length)

    ;; Update timestamps.
    (minf (chnk-start buffer) timestamp*)
    (maxf (chnk-end buffer) timestamp*)

    ;; Make or reuse a `chunk-entry' instance in the entries vector of
    ;; BUFFER.
    (make-or-reuse-instance
     (chnk-entries buffer) chunk-entry
     :channel-id channel
     :timestamp  timestamp*
     :size       length
     :entry      entry)

    ;; Update index. Abuse chnk-count for tracking offsets
    (put-entry index timestamp* (chnk-count buffer) (chnk-chunk-id buffer))
    (incf (chnk-count buffer) (+ 16 length)))) ; TODO(jmoringe): constants

(defmethod get-entry ((file    file)
                      (channel integer)
                      (index   integer))
  (let+ (((&accessors (stream backend-stream)
                      (lock   rsbag.backend::backend-lock))
          file)
         (index1 (channel-index (gethash channel (file-%channels file)))) ; TODO(jmoringe): make a method?
         (offset (index-offset index1 index)))
    (bt:with-lock-held (lock)
      (file-position stream (+ offset 12))
      (let ((length (nibbles:read-ub32/le stream)))
        (read-chunk-of-length length stream)))))

;;; Buffering

(defmethod make-buffer ((file     file)
                        (previous (eql nil)))
  (make-buffer file (make-instance 'chnk
                                   :compression 0
                                   :entries     (make-array 0
                                                            :adjustable   t
                                                            :fill-pointer 0))))

(defmethod make-buffer ((file     file)
                        (previous chnk))
  (let+ (((&accessors (next-chunk-id file-%next-chunk-id)) file))
    (setf (fill-pointer (chnk-entries previous)) 0)
    (reinitialize-instance previous
                           :chunk-id (incf next-chunk-id)
                           :start    (1- (ash 1 64))
                           :end      0
                           :count    0)))

(defmethod write-buffer ((file   file)
                         (buffer chnk))
  (let+ (((&structure-r/o backend- stream) file)
         ((&structure chnk- count entries) buffer))
    ;; We abused chnk-count to store the size of the chunk instead of
    ;; the number of entries. Correct this before writing the chunk.
    (unless (zerop count)
      (setf count (length entries))
      (bt:with-lock-held ((rsbag.backend::backend-lock file))
        (pack buffer stream)
        (force-output stream))

      ;; For the sake of conservative garbage collectors, we
      ;; dereference as much as possible here. On SBCL we even garbage
      ;; collect explicitly.
      (let ((empty (load-time-value (nibbles:octet-vector) t)))
        (map nil (lambda (entry)
                   (setf (chunk-entry-entry entry) empty))
             entries)))))

(defmethod buffer-property ((backend file)
                            (buffer  chnk)
                            (name    (eql :length/entries)))
  (length (chnk-entries buffer)))

(defmethod buffer-property ((backend file)
                            (buffer  chnk)
                            (name    (eql :length/bytes)))
  (chnk-count buffer))

;;; Utility functions

(defun maybe-write-header (stream)
  ;; Write a header when the file length is zero or STREAM is not
  ;; associated with a file (in which case a `type-error' is
  ;; signaled).
  (when (handler-case (zerop (file-length stream)) (type-error () t))
    (pack (make-instance 'tide
                         :version-major +format-version-major+
                         :version-minor +format-version-minor+
                         :num-channels  0
                         :num-chunks    0)
          stream)))

(defun encode-type (type)
  "Encode the keyword or list TYPE as a channel type string."
  (etypecase type
    (null    "")
    (list    (let ((*package*   (find-package '#:keyword))
                   (*readtable* (copy-readtable)))
               (setf (readtable-case *readtable*) :invert)
               (format nil "~{~A~^:~}" type)))
    (keyword (string type))))

(defun decode-type (type)
  "Decode the channel type string TYPE as nil, a keyword of a list of
   type information."
  (cond
    ((emptyp type)
     nil)
    ((find #\: type)
     (let+ (((class-name &rest arg-strings) (split-sequence #\: type))
            (*package*   (find-package '#:keyword))
            (*readtable* (copy-readtable))
            (class       (progn
                           (setf (readtable-case *readtable*) :invert)
                           (read-from-string class-name)))
            (args        (map 'list #'read-from-string arg-strings)))
       (cons class args)))
    (t
     (make-keyword type))))

(defun %chunk-id->offset (id chunks
                          &optional
                          (start 0)
                          (end   (length chunks)))
  (declare (type (unsigned-byte 32) id start end)
           (type (simple-array (cons (unsigned-byte 32) (unsigned-byte 64)) (*)) chunks))
  (labels ((rec (start end)
             (declare (type (unsigned-byte 32) start end))
             (when (= start end)
               (error "~@<Referenced chunk with id ~:D is not in chunk ~
                       list.~@:>"
                      id))

             (let* ((pivot  (ash (+ start end) -1))
                    (pivot* (car (aref chunks pivot))))
               (cond
                 ((< pivot* id)
                  (rec (1+ pivot) end))
                 ((> pivot* id)
                  (rec start pivot))
                 (t
                  (cdr (aref chunks pivot)))))))
    (rec start end)))
