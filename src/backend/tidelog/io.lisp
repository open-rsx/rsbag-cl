;;;; io.lisp --- Input and output of TIDE log structures.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; Scan

(define-condition-translating-method scan ((source t) (object t)
                                           &optional start)
  (((and error (not tidelog-condition)) invalid-tidelog-structure
    :var           condition
    :cause-initarg nil)
   :source           source
   :format-control   "~@<Failed to ~A for block ~A~@[ at position ~
                      ~/rsbag.backend:print-offset/~]: ~A~@:>"
   :format-arguments (list 'scan object (when (streamp source)
                                          (file-position source))
                           condition)))

(defmethod scan :before ((source stream) (object t)
                         &optional start)
  ;; Seek to position START before starting to scan.
  (when start
    (file-position source start)))

(defmethod scan ((source stream) (object (eql :tide))
                 &optional start)
  (declare (ignore start))

  ;; Consume and check the TIDE block.
  (let ((block (unpack source :block)))
    (unless (typep block 'tide)
      (error "~@<Starts with a ~A block instead of a ~A block~@:>"
             (class-name (class-of block)) 'tide))
    (let+ (((&accessors-r/o (major tide-version-major)
                            (minor tide-version-minor)) block))
      (log:info "~@<Read ~A block with version ~D.~D~@:>"
                'tide major minor)
      (unless (= +format-version-major+ major)
        (cerror "Try to process the file anyway."
                "~@<Cannot process format version ~D.~D (major version ~
                 is different from ~D.~D)~@:>"
                major minor +format-version-major+ +format-version-minor+))))

  ;; Scan through remaining blocks.
  (function-calling-restart-bind
      (((retry () retry)
        :report (lambda (stream)
                  (format stream "~@<Retry reading at the same ~
                                  position in ~A.~@:>"
                          source)))
       ((continue (&optional condition) skip bail)
        :report (lambda (stream)
                  (format stream
                          (if skip
                              "~@<Skip ahead to the next undamaged ~
                               block in ~A.~@:>"
                              "~@<Do not scan the remainder of ~
                               ~A.~@:>")
                          source)))
       ((abort (&optinal condition) bail)
        :report (lambda (stream)
                  (format stream "~@<Do not scan the remainder of ~
                                  ~A.~@:>"
                          source))))
    (iter (with complete? = t)
          (when (first-iteration-p)
            (setf retry (lambda () (next-iteration))
                  bail  (lambda (&optional condition)
                          (declare (ignore condition))
                          (setf complete? nil)
                          (return (values channels indices chunks complete?)))
                  skip  (lambda (&optional condition)
                          (declare (ignore condition))
                          (let ((skip/old skip)) ; prevent recursion
                            (setf skip      nil
                                  complete? nil)
                            (find-next-block source)
                            (setf skip skip/old))
                          (next-iteration))))
          (while (listen source))
          (let+ (((&values offset block) (scan source :block)))
            (typecase block
              (chan
               (collect block               :into channels))
              ((cons (eql indx) integer) ; cdr is channel id
               (collect (cons (cdr block) offset) :into indices))
              ((cons (eql chnk) integer) ; cdr is CHNK id
               (collect (cons (cdr block) offset) :into chunks))))
          (finally (return (values channels indices chunks complete?))))))

(defmethod scan ((source stream) (object (eql :block))
                 &optional start)
  (declare (ignore start))

  (let+ ((offset (file-position source))
         ((&values class length) (unpack source :block-header))
         (class-name (class-name class)))
    (values
     offset
     (case class-name
       ((type1 chan)
        (unpack (read-chunk-of-length length source)
                (allocate-instance class)))
       ((indx chnk)
        (prog1
            (cons class-name (nibbles:read-ub32/le source)) ; CHNK id / INDX channel id
          (file-position source (+ (file-position source) (- length 4)))))
       (t
        (file-position source (+ (file-position source) length)))))))

;;; Unpacking

(define-condition-translating-method unpack ((source t) (object t)
                                             &optional start)
  (((and error (not tidelog-condition)) invalid-tidelog-structure
    :var condition)
   :source           source
   :format-control   "~@<Failed to ~A block ~A~@[ at position ~
                      ~/rsbag.backend:print-offset/~]: ~A~@:>"
   :format-arguments (list 'unpack object (when (streamp source)
                                            (file-position source))
                           condition)))

(defmethod unpack :before ((source stream) (object t)
                           &optional start)
  ;; Seek to position START before unpacking into OBJECT.
  (when start
    (file-position source start)))

(defmethod unpack ((source stream) (object (eql :block-header))
                   &optional start)
  (declare (ignore start))

  (let ((header (read-chunk-of-length 12 source)))
    (declare (dynamic-extent header))
    (handler-bind
        ((error (lambda (condition)
                  (error "~@<Could not decode header~:@_~
                          ~<| ~/rsbag:print-hexdump/~:>~:@_~
                          ~A~:>"
                         (list header) condition))))
      (values (byte-pattern->block-class (subseq header 0 4))
              (nibbles:ub64ref/le header 4)))))

(defmethod unpack ((source stream) (object (eql :block))
                   &optional start)
  (declare (ignore start))

  (let+ (((&values class length) (unpack source :block-header))
         (file-length (ignore-errors (file-length source)))) ; TODO cache this when we have the stream abstraction
    (when (and file-length
               (> (+ (file-position source) length) file-length))
      (cerror "Try to read the block anyway"
              "~@<Bounds [~/rsbag.backend:print-offset/, ~
               ~/rsbag.backend:print-offset/[ of ~A block would be ~
               outside bounds [~/rsbag.backend:print-offset/, ~
               ~/rsbag.backend:print-offset/[ of ~A.~@:>"
              (file-position source) (+ (file-position source) length)
              (class-name class)
              0 (file-length source) source))
    (unpack (read-chunk-of-length (if (eq (class-name class) 'tide) 10 length) source) ; TODO(jmoringe): hack
            (allocate-instance class))))

;;; Packing
;;;
;;; Nothing to do since everything is handled by the methods generated
;;; in spec.lisp
