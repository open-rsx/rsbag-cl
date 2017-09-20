;;;; file.lisp --- High-level tests for the tidelog backend.
;;;;
;;;; Copyright (C) 2013, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog.test)

(addtest (backend-tidelog-root
          :documentation
          "Test retained entry order in case of identical
           timestamps within a single channel.")
  repeated-timestamp

  (let+ ((channel-name "foo")
         (timestamp    (local-time:now))
         (entry-1      (nibbles:octet-vector 1 2 3 4))
         (entry-2      (nibbles:octet-vector 5 6 7 8))
         ((&flet write-bag ()
            (with-writable-log (backend)
              (let ((channel (rsbag.backend:make-channel-id backend channel-name)))
                (rsbag.backend:put-channel backend channel channel-name '())
                (rsbag.backend:put-entry backend channel timestamp entry-1)
                (rsbag.backend:put-entry backend channel timestamp entry-2)
                channel))))
         ((&flet read-bag (data channel)
            (with-readable-log (backend data)
              (values (rsbag.backend:get-entry-at-index backend channel 0)
                      (rsbag.backend:get-entry-at-index backend channel 1)))))
         ((&values channel data) (write-bag))
         ((&values entry-1* entry-2*) (read-bag data channel)))
    (ensure-same entry-1* entry-1 :test #'equalp)
    (ensure-same entry-2* entry-2 :test #'equalp)))
