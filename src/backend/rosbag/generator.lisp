;;;; generator.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; General notes regarding the file format (taken from the Rosbag
;;;; documentation obtained from
;;;; http://www.ros.org/wiki/Bags/Format/2.0)
;;;;
;;;; Records
;;;;
;;;; Each record has the following format:
;;;;
;;;; <header_len><header><data_len><data>
;;;;
;;;; Each record header (the header field that precedes the data
;;;; field) contains a sequence of name=value fields, formatted as
;;;; follows:
;;;;
;;;;   <field1_len><field1_name>=<field1_value><field2_len><field2_name>=<field2_value>...<fieldN_len><fieldN_name>=<fieldN_value>
;;;;
;;;; The total length of fieldX_name=fieldX_value, including the '='
;;;; character, is fieldX_len bytes. The total length of this header
;;;; sequence (including the names, '=' characters, value lengths, and
;;;; values) is header_len bytes.
;;;;
;;;; Header fields may appear in any order. Field names can contain
;;;; any printable ASCII character (0x20 - 0x7e), except '='
;;;; (0x3d). Field values can contain any data (including binary data
;;;; with embedded nulls, newlines, etc.)
;;;;
;;;; Every header is required to include the following field:
;;;;
;;;; Name Description                        Format        Length
;;;; op   record type identifier (see below) unsigned byte 1 byte
;;;;
;;;; The op field is used to distinguish between different types of
;;;; record.

(cl:in-package #:rsbag.backend.rosbag)

;;; Class generator

(defun accessor-name (class-name field-name)
  "TODO(jmoringe): document"
  (symbolicate class-name "-" (case field-name
                                (&data '#:data)
                                (t     field-name))))

(defun specs->class (name specs &key documentation)
  `(defclass ,name ()
     (,@(mapcar (curry #'spec->slot name) specs))
     ,@(when documentation
         `((:documentation ,documentation)))))

(defun spec->slot (class-name spec)
  (let+ (((name type &key documentation &allow-other-keys) spec)
         (type          (type-spec->lisp-type type)))
    `(,name :initarg  ,(make-keyword name)
            :type     ,type
            :accessor ,(accessor-name class-name name)
            ,@(when documentation
                `(:documentation ,documentation)))))

(defun type-spec->lisp-type (spec)
  (typecase spec
    ((cons (eql :blob) list) ;; TODO which one?
     'nibbles:octet-vector)
    ((eql :blob)
     'nibbles:octet-vector)
    ((cons (eql :repeated) list)
     'vector)
    ((cons (eql :compressed) (cons symbol t))
     (type-spec->lisp-type (third spec)))
    (t
     spec)))

;;; Size method

(defun specs->size (class-name specs)
  `(defmethod size ((object ,class-name))
     (+ ,@(mapcar (rcurry #'spec->size class-name 'object)
                  specs))))

(defun spec->size (spec class-name object)
  (let+ (((name type &rest &ign) spec)
         (accessor-name (symbolicate class-name "-" name)))
    (type-spec->size type `(,accessor-name ,object))))

(defun type-spec->size (type-spec value)
  (etypecase type-spec
    ((cons symbol t)
     (destructuring-ecase type-spec
       ((unsigned-byte size)
        (/ size 8))

       #+no ((:string length-type)
        `(+ ,(type-spec->size length-type :unused)
            (length ,value)))

       ((:blob length-slot)
        (declare (ignore length-slot))
        `(length ,value))

       ((:repeated #+no count-slot sub-type)
        #+no (declare (ignore count-slot))
        `(+ #+no ,(type-spec->size '(unsigned-byte 32) :unused)
            (iter (for val each ,value)
                  (summing ,(type-spec->size sub-type 'val)))))

       ((:detect)
        '(error "not implemented"))

       ((:compressed compression-slot sub-type)
        (declare (ignore compression-slot sub-type))
        '(error "not implemented"))))

    (symbol
     `(size ,value))))

;;; Deserializer

(defun specs->deserializer (class-name specs)
  `(defmethod unpack ((source simple-array) (object ,class-name)
                      &optional
                      (start 0))
     (check-type source simple-octet-vector)

     (let+ ((offset start)
            (header-length
             ,(type-spec->deserializer '(unsigned-byte 32) 'source 'offset)))

       ;; Process header fields.
       (iter (while (< (- offset start) header-length))
             (format t "~D of ~D~%" (- offset start) header-length)
             (let+ ((field-length
                     ,(type-spec->deserializer '(unsigned-byte 32) 'source 'offset))
                    (equals-offset (progn
                                          (format t "Searching ~S in ~S~%"
                                                  #\= (subseq source offset (+ offset field-length)))
                                   (position (char-code #\=) source
                                             :start offset
                                             :end   (+ offset field-length))))
                    (value-offset (progn
                                    (when (not equals-offset)
                                      (error "Field of length 0")
                                      (return))
                                    (1+ equals-offset)))
                    (name (sb-ext:octets-to-string source
                                                   :start offset
                                                   :end   equals-offset)))
               (cond
                 ,@(mapcar
                    (lambda+ ((name type
                                    &key
                                    (id (string-downcase name))
                                    &allow-other-keys))
                      (let ((accessor-name (symbolicate class-name "-" name)))
                        `((string= name ,id)
                          (format t "Extracting ~A from ~D to ~D~%" ,(string name) value-offset (+ offset field-length))
                          (setf (,accessor-name object)
                                ,(type-spec->deserializer type 'source 'value-offset '(+ offset field-length))))))
                    (remove '&data specs :key #'first))
                 (t
                  (format t "Ignoring ~S from ~D to ~D~%" name offset (+ value-offset field-length))))
               (incf offset field-length)))

       ;; Process data block.
       ,@(when-let ((data (find '&data specs :key #'first)))
           (let+ (((&ign type &key &allow-other-keys) data)
                  (accessor-name (symbolicate class-name '#:-data)))
             `((let+ ((data-length
                      ,(type-spec->deserializer '(unsigned-byte 32) 'source 'offset))
                      (end (+ offset data-length)))
                (format t "data length ~D ~:*~X~%" data-length)
                (format t "Setting slot ~S~%" ',accessor-name)
                (setf (,accessor-name object)
                      (when (plusp data-length)
                        ,(type-spec->deserializer type 'source 'offset 'end)))))))


       (values object (- offset start)))))

(defun+ spec->deserializer/header ((name type &rest nil) class-name source object offset)
  `(let+ (((&values value length)
           ,(type-spec->deserializer type source offset)))
     (declare (type ,(type-spec->lisp-type type) value))
     (setf (,(accessor-name class-name name) ,object) value)
     (incf ,offset length)))

(defun type-spec->deserializer (type-spec source offset &optional end)
  (etypecase (ensure-list type-spec)
    ((cons (or keyword (member string unsigned-byte)) list)
     (format t "destr ~S~%" type-spec)
     (destructuring-ecase (ensure-list type-spec)
       ((unsigned-byte size)
        `(prog1
             ,(ecase size
                (8  `(aref ,source ,offset))
                (32 `(nibbles:ub32ref/le ,source ,offset))
                (64 `(nibbles:ub64ref/le ,source ,offset)))
           (incf ,offset ,(/ size 8))))

       ((string)
        `(prog1
             (sb-ext:octets-to-string
              source :start ,offset :end ,end #+no (+ offset field-length) #+no (+ offset length))
           (setf ,offset ,end))

        #+no `(let+ (((&values length length-length)
                      ,(type-spec->deserializer length-type source offset))
                     (data-offset (+ ,offset length-length)))
                (values (sb-ext:octets-to-string
                         source :start data-offset :end (+ data-offset length))
                        (+ length-length length))))

       ((:blob #+no length-slot)
        #+no`(let* ((length (slot-value object ',length-slot))) ; TODO(jmoringe): slot access
               (values (subseq ,source ,offset (+ ,offset length)) length))
        `(prog1
             (subseq ,source ,offset ,end)
           (setf ,offset ,end)))

       ((:repeated #+no count-slot sub-type)
        `(iter (format t "Repeated at offset ~D of ~D~%" ,offset ,end)
               (while (< ,offset ,end))
               (restart-case
                   (let ((value ,(type-spec->deserializer sub-type source offset end)))
                     (collect value :result-type vector))
                 (continue ()
                   :report (lambda (stream)
                             (format stream "~@<Ignore the rest of the ~
~S chunk at (local offset) ~:D and continue processing.~@:>"
                                     ',type-spec ,offset))
                   (terminate)))))

       ((:detect)
        `(let+ ((header-length
                 ,(type-spec->deserializer '(unsigned-byte 32) source offset))
                (opcode (extract-opcode ,source ,offset (+ ,offset header-length)))
                ((&values object length)
                 (progn
                   (format t "Found header of length ~D with opcode ~D ~%" header-length opcode)
                   (unpack ,source (allocate-instance
                                    (find-class (find-record-class opcode)))
                           (- ,offset 4)))))

           (incf offset (- length 4))
           object))

       ((:compressed compression-slot sub-type)
        `(let ((compression "none" #+later (slot-value object ',compression-slot)))
           (cond
             ((string= compression "none")
              ,(type-spec->deserializer sub-type source offset end))
             (t
              (error "~@<Compression method ~S is not supported.~@:>"
                     compression)))))))

    ((cons symbol null)
     `(let+ ((object (allocate-instance (find-class ',type-spec)))
             ((&values result length) (unpack ,source object ,offset)))
        (incf ,offset length)
        result))))

;;; Serializer

(defun specs->serializer (class-name specs)
  `(defmethod pack ((object ,class-name) (source simple-array)
                    &optional
                    (start 0))
     (check-type source nibbles:octet-vector)

     (let ((offset start))
       ,@(map 'list (rcurry #'spec->serializer
                            class-name 'source 'object 'offset)
              specs)
       (- offset start))))

(defun spec->serializer (spec class-name source object offset)
  (let+ (((name type &rest &ign) spec)
         (accessor-name (symbolicate class-name "-" name)))
        `(let ((value (,accessor-name ,object)))
           (declare (type ,(type-spec->lisp-type type) value))
           (incf offset ,(type-spec->serializer type 'value source offset)))))

(defun type-spec->serializer (type-spec value source offset)
  (etypecase type-spec
    ((cons (or keyword (member unsigned-byte)) list)
     (destructuring-ecase type-spec
       ((unsigned-byte size)
        `(progn
           ,(ecase size
              (8  `(setf (aref ,source ,offset) ,value))
              (32 `(setf (nibbles:ub32ref/le ,source ,offset) ,value))
              (64 `(setf (nibbles:ub64ref/le ,source ,offset) ,value)))
           ,(/ size 8)))

       #+no ((:string length-type)
        `(let ((offset* ,offset))
           (incf offset* ,(type-spec->serializer
                           length-type `(length ,value) source 'offset*))
           (let ((octets (sb-ext:string-to-octets ,value)))
             (replace ,source octets :start1 offset*)
             (incf offset* (length octets)))
           (- offset* ,offset)))

       ((:blob length-slot)
        (declare (ignore length-slot))
        `(progn
           (setf (subseq ,source ,offset (+ ,offset (length ,value)))
                 value)
           (length ,value)))

       ((:repeated #+no count-slot sub-type)
        #+no (declare (ignore count-slot))
        `(iter (for val each value)
               (with offset* = ,offset)
               (incf offset* ,(type-spec->serializer sub-type 'val source 'offset*))
               (finally (return (- offset* ,offset)))))

       ((:compressed compression-slot sub-type)
        (declare (ignore compression-slot sub-type))
        `(error "not implemented"))

       ))

    ((eql :detect)
     `(error "not implemented"))

    (symbol
     `(pack ,value ,source ,offset))))
