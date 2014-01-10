;;;; io.lisp ---
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag)

;;; Scan

(define-condition-translating-method scan ((source t) (object t)
                                           &optional start)
  (((and error (not rosbag-condition)) invalid-rosbag-structure
    :var           condition
    :cause-initarg nil)
   :source           source
   :format-control   "~@<Failed to ~A for record ~A~@[ at position ~
                      ~/rsbag.backend:print-offset/~]: ~A~@:>"
   :format-arguments (list 'scan object (when (streamp source)
                                          (file-position source))
                           condition)))

(defmethod scan :before ((source stream) (object t)
                         &optional start)
  ;; Seek to position START before starting to scan.
  (when start
    (file-position source start)))

(defmethod scan ((source stream) (object (eql :rosbag))
                 &optional (start 0))
  ;; Consume marker at beginning of file.
  (let* ((buffer   (make-octet-vector (length +header/2.0+)))
         (consumed (read-sequence buffer source)))
    (unless (= consumed (length +header/2.0+))
      (error "~@<~A is only ~:D byte~:P long; too short to be a Rosbag ~
              file.~@:>"
             source (+ start consumed)))
    (when (not (equalp buffer +header/2.0+))
      (cerror "Try to process the data anyway."
              "~@<Encountered~@:_~
               ~<| ~@;~/rsbag:print-hexdump/~:>~@:_
               instead of header~@:_
               ~<| ~@;~/rsbag:print-hexdump/~:>~@:>"
              (list buffer) (list +header/2.0+))))

  ;; Read header.
  (let ((header (unpack source :record)))
    ;; TODO do something with header?
    )

  ;; TODO Scan through remaining records.?
  #+no (iter (while (listen source))
             ))

;;; Unpack

(define-condition-translating-method unpack ((source t) (object t)
                                             &optional start)
  (((and error (not rosbag-condition)) invalid-rosbag-structure
    :var condition)
   :source           source
   :format-control   "~@<Failed to unpack record ~A~@[ at position ~
                      ~/rsbag.backend:print-offset/~]: ~A~@:>"
   :format-arguments (list object (when (streamp source)
                                    (file-position source))
                           condition)))

(defmethod unpack :before ((source stream) (object t)
                           &optional start)
  ;; Seek to position START before unpacking into OBJECT.
  (when start
    (file-position source start)))

(defmethod unpack ((source stream) (object (eql :record))
                   &optional start)
  (declare (ignore start))
  (let* ((header-length (read-ub32/le source))
         (header-buffer (read-chunk-of-length header-length)) ; TODO currently in TIDELog backend
         (opcode        (extract-opcode header-buffer))
         (data-length   (read-ub32/le source))
         (data-buffer   (read-chunk-of-length data-length)))
    ;; TODO generated unpack method is not yet prepared for this cons trick
    (unpack (cons header-buffer data-buffer)
            (allocate-instance (find-record-class opcode)))))

;;; Utilities

(defun extract-opcode (buffer &optional (start 0 ) end)
  (declare (type simple-octet-vector buffer))
  ;; TODO(jmoringe, 2013-01-24): hack
  (let ((position (search (load-time-value
                           (concatenate 'simple-octet-vector
                                        #(4 0 0 0)
                                        (map 'list #'char-code "op=")))
                          buffer :start2 start :end2 end)))
    (aref buffer (+ position 7))))
