;;;; io.lisp ---
;;;;
;;;; Copyright (C) 2013 Jan Moringen
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
   :format-control   "~@<Failed to scan for record ~A~@[ at position ~
                      ~:D~]: ~A~@:>"
   :format-arguments (list object
                           (when (streamp source)
                             (file-position source))
                           condition)))

(defmethod scan :before ((source stream) (object t)
                         &optional start)
  "Seek to position START before starting to scan."
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
              "~@<Encountered ~S instead of header (~S).~@:>"
              (make-safe-string buffer) (make-safe-string +header/2.0+))))

  ;; Read header.
  (unpack source :record)

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
                      ~:D~]: ~A~@:>"
   :format-arguments (list object
                           (when (streamp source)
                             (file-position source))
                           (format nil "~A" condition))))

(defmethod unpack :before ((source stream) (object t)
                           &optional start)
  "Seek to position START before unpacking into OBJECT."
  (when start
    (file-position source start)))

(defmethod unpack ((source stream) (object (eql :record))
                   &optional start)
  "Seek to position START before unpacking into OBJECT."
  (declare (ignore start))
  (let* ((header-length (read-ub32/le source))
         (header-buffer (let ((buffer (make-octet-vector (+ 4 header-length))))
                          (unless (= (read-sequence buffer source :start 4) (+ 4 header-length))
                            (error "~@<Failed to read header~@:>"))
                          (setf (ub32ref/le buffer 0) header-length)
                          buffer))
         (opcode        (extract-opcode header-buffer 4 (+ 4 header-length)))
         (data-length   (read-ub32/le source))
         (buffer        (let ((buffer (make-octet-vector (+ 4 header-length 4 data-length))))
                          (setf (subseq buffer 0 (+ 4 header-length)) header-buffer)
                          (unless (= (read-sequence buffer source
                                                    :start (+ 4 header-length 4))
                                     (+ 4 header-length 4 data-length))
                            (error "~@<Failed to read data~@:>"))
                          (setf (ub32ref/le buffer (+ 4 header-length)) data-length)
                          buffer)))
    (print (file-position source))
    (unpack buffer (allocate-instance (find-class (find-record-class opcode))))))

;;; Utilities

(defun make-safe-string (buffer)
  (substitute-if #\. (curry #'> 32)
                 (sb-ext:octets-to-string
                  buffer :external-format '(:ascii :replacement #\?))
                 :key #'char-code))

(defun extract-opcode (buffer start end)
  ;; TODO(jmoringe, 2013-01-24): hack
  (let ((position (search (load-time-value
                           (concatenate 'simple-octet-vector
                                        #(4 0 0 0)
                                        (map 'list #'char-code "op=")))
                          buffer :start2 start :end2 end)))
    (aref buffer (+ position 7))))
