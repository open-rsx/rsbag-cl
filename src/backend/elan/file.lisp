;;;; file.lisp --- Elan file format support.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.elan)

(defclass file (stream-mixin
                direction-mixin)
  ((author          :type     (or null string)
                    :accessor file-%author
                    :initform nil
                    :documentation
                    "Stores the author of the file.")
   (channels        :type     list
                    :reader   get-channels
                    :accessor file-%channels
                    :initform '()
                    :documentation
                    "Stores information of the channels (or tiers,
                     rather) present in the file. Entries are of the
                     form

                       (ID NAME META-DATA)

                     where ID is the numeric id of the channel, NAME
                     is the name as string and META-DATA is a plist of
                     additional data associated to the channel.")
   (data            :type     hash-table
                    :reader   file-%data
                    :initform (make-hash-table :test #'eq)
                    :documentation
                    "Maps channel ids to channel data. Each entry of a
                     channel is of the form

                       (START END DATUM)

                     where START and END are the start and end
                     timestamps respectively and DATUM is the (string)
                     datum of the entry.")
   (document        :type     stp:document
                    :accessor file-%document
                    :documentation
                    "Stores the `stp:document' instance which contains
                     the DOM representation of the file. The document
                     is not updated continuously but only on
                     write-back.")
   (next-channel-id :type     non-negative-integer
                    :accessor file-%next-channel-id
                    :initform 0
                    :documentation
                    "Stores the id that will be assigned to the next
                     new channel."))
  (:documentation
   "Instances of this class represent Elan eaf-files. All data is
    serialized and written or read and deserialized when the file is
    written or read respectively."))

(service-provider:register-provider/class
 'backend :eaf :class 'file)

(defmethod shared-initialize :after ((instance   file)
                                     (slot-names t)
                                     &key)
  (let+ (((&accessors (direction       backend-direction)
                      (stream          backend-stream)
                      (document        file-%document)
                      (author          file-%author)
                      (channels        file-%channels)
                      (data            file-%data)
                      (next-channel-id file-%next-channel-id)) instance)
         (urls       '())
         (time-slots '())
         (tiers      '())
         (base-time  (local-time:now))
         ((&flet resolve (id)
            (+ (timestamp->millisecs base-time)
               (cdr (assoc id time-slots :test #'string=))))))

    (cond
      ;; Data is available - parse as XML document.
      ((and (member direction '(:input :io)) (listen stream))
       (setf document (parse/keep-open stream (stp:make-builder)))
       (setf (values author base-time urls time-slots tiers)
             (values-list
              (xloc:xml-> (stp:document-element document) 'file/list))))

      ;; No data is available, but direction implies output -
      ;; create an empty XML document and write it back later.
      ((member direction '(:output :io))
       (setf document (stp:make-document
                       (stp:make-element "ANNOTATION_DOCUMENT"))))

      ;; No data is available and direction does not imply
      ;; output - signal an error.
      (t
       (error "~@<Trying to read an empty EAF file from: ~S~@:>"
              stream)))

    ;; Add video channels.
    (iter (for url each urls :with-index i)
          (let* ((name (format nil "video~D" i))
                 (id   (make-channel-id instance name)))
            (push (list id name `(:type (,rsbag.transform:+rsb-schema-name+
                                         :|.rst.vision.Image|)))
                  channels)))

    ;; Add annotation channels.
    (iter (for (name linguistic-type-ref content) in tiers)
          (let ((id (make-channel-id instance name)))
            (push (list id name `(:type            :utf-8-string
                                  :linguistic-type ,linguistic-type-ref))
                  channels)
            (setf (gethash id data)
                  (map 'list (lambda+ ((&ign start end datum))
                               (list (resolve start) (resolve end) datum))
                       content))))))

(defmethod close ((file file) &key abort)
  "TODO(jmoringe): document"
  (declare (ignore abort))

  (when (member (backend-direction file) '(:output :io))
    (let+ (((&accessors-r/o (stream   backend-stream)
                            (document file-%document)
                            (author   file-%author)
                            (channels file-%channels)
                            (data     file-%data)) file)
           (current-time-slot-id 0)
           ((&flet make-time-slot-id ()
              (format nil "ts~D" (incf current-time-slot-id))))
           (time-slots (make-hash-table :test #'eql))
           ((&flet time-slot (timestamp)
              (ensure-gethash timestamp time-slots (make-time-slot-id))))
           (current-annotation-id 0)
           ((&flet make-annotation-id ()
              (format nil "as~D" (incf current-annotation-id))))
           (tiers (iter (for (id name meta-data) in   channels)
                        (for entries             next (gethash id data))
                        (collect (list name (getf meta-data :linguistic-type "Default")
                                       (map 'list (lambda+ ((start end datum))
                                                    (list (make-annotation-id)
                                                          (time-slot start)
                                                          (time-slot end)
                                                          datum))
                                            (sort (coerce entries 'vector) #'< :key #'first))))))
           (base-time (or (extremum (hash-table-keys time-slots) #'<) 0))
           (time-slots/cons (iter (for (timestamp id) in-hashtable time-slots)
                                  (collect (cons id (- timestamp base-time))))))
      (xloc:->xml
       (list (or author "") (millisecs->timestamp base-time) '() time-slots/cons tiers) ; TODO(jmoringe, 2011-12-01): media stuff
       (stp:document-element document) 'file/list)
      (file-position stream 0)
      (serialize/keep-open document stream)))
  (when (next-method-p)
    (call-next-method)))

(defmethod make-channel-id ((backend file)
                            (name    string))
  (prog1
      (file-%next-channel-id backend)
    (incf (file-%next-channel-id backend))))

(defmethod put-channel ((backend   file)
                        (channel   integer)
                        (name      string)
                        (meta-data list))
  (let+ (((&plist-r/o (type :type :utf-8-string)) meta-data)
         ((&accessors (channels file-%channels)) backend))
    (unless (eq type :utf-8-string)
      (error "~@<Cannot handle channel type ~S; only ~S is supported.~@:>"
             type :utf-8-string))

    (push (list channel name (append (remove-from-plist meta-data :type)
                                     (list :type type)))
          channels)))

(defmethod get-num-entries ((file    file)
                            (channel integer))
  (length (gethash channel (file-%data file))))

(defmethod get-timestamps ((file    file)
                           (channel integer))
  (map 'list (compose #'millisecs->timestamp #'car)
       (gethash channel (file-%data file))))

(defmethod put-entry ((file      file)
                      (channel   integer)
                      (timestamp local-time:timestamp)
                      (entry     string))
  (let+ (((&accessors-r/o (data  file-%data)) file)
         (timestamp* (timestamp->millisecs timestamp)))
    (push (list timestamp* timestamp* entry)
          (gethash channel data))))

(defmethod get-entry ((file    file)
                      (channel integer)
                      (index   integer))
  (third (nth index (gethash channel (file-%data file)))))
