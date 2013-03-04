;;;; generator.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag.test)

(with-input-from-file (stream "/homes/jmoringe/code/cor-lab/rsbag/rsbag-cl/material/bags/BagFiles_ist_TuGraz/DeviceShutDownExperiment_IstTuGraz.bag"
                              :element-type '(unsigned-byte 8))


  (list
   (progn
     (file-position stream #xd)
     (let ((buffer (nibbles:make-octet-vector 100000)))
       (read-sequence buffer stream)
       (setf cl-user::*chunk*
             (unpack buffer (allocate-instance (find-class 'rsbag.backend.rosbag::bag-header))))))
   #+no (setf cl-user::*chunk*
	 (progn
	   (file-position stream #x1015)
	   (let ((buffer (nibbles:make-octet-vector 1000000)))
	     (read-sequence buffer stream)
	     (unpack buffer (allocate-instance (find-class 'rsbag.backend.rosbag::chunk))))))
   #+no (progn
     (file-position stream #x1046)
     (let ((buffer (make-octet-vector 100000)))
       (read-sequence buffer stream)
       (describe (unpack buffer (make-instance 'connection)))))
   #+no (print (make-string 70 :initial-element #\=))
   #+no (progn
     (file-position stream #x14f5)
     (let ((buffer (make-octet-vector 100000)))
       (read-sequence buffer stream)
       (describe (unpack buffer (make-instance 'message-data)))))
   #+no (print (make-string 70 :initial-element #\=))
   #+no (progn
     (file-position stream #x156f)
     (let ((buffer (make-octet-vector 100000)))
       (read-sequence buffer stream)
       (describe (unpack buffer (make-instance 'connection)))))))


(defun make-file ()
 (with-input-from-file (stream (asdf:system-relative-pathname
                                :cl-rsbag "material/bags/BagFiles_ist_TuGraz/DeviceShutDownExperiment_IstTuGraz.rosbag")
                               :element-type '(unsigned-byte 8))
   (let ((*standard-output* (make-broadcast-stream)))
     (make-instance 'rsbag.backend.rosbag::file
                    :direction :input
                    :stream    stream))))

(deftestsuite rsbag.backend.rosbag.io-root (rsbag.backend.rosbag.root)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(addtest (rsbag.backend.rosbag.io-root
          :documentation
          "TODO(jmoringe): document")
  rosbag-scan/smoke

  (ensure-condition 'error ;; TODO
    (with-input-from-sequence (stream (octet-vector))
      (scan stream :rosbag)))

  (ensure-condition 'error ;; TODO
    (with-input-from-sequence
        (stream (octet-vector 65 66 67 4 5 6 7 8 9 10 11 12 13))
      (scan stream :rosbag))))
