;;;; builder.lisp --- Unit tests for builder support.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.builder.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:lift

   #:rsbag

   #:rsbag.test)

  (:import-from #:architecture.builder-protocol.test
   #:record-un-build-calls/peeking)

  (:export
   #:rsbag-builder-root)

  (:documentation
   "This package contains test for the builder module."))

(cl:in-package #:rsbag.builder.test)

(deftestsuite rsbag-builder-root ()
  ()
  (:documentation
   "Unit tests for builder support."))

(defun replace-timestamps (tree replacements)
  (reduce (lambda+ (tree (new . old))
            (subst-if new (conjoin (of-type 'local-time:timestamp)
                                   (curry #'local-time:timestamp= old))
                      tree))
          replacements :initial-value tree))

(defun check-un-build-calls (builder atom-type timestamp-replacements cases)
  (mapc (lambda+ ((object expected-calls))
          (let+ (((&values &ign calls)
                  (architecture.builder-protocol:with-unbuilder
                      (builder builder)
                   (record-un-build-calls/peeking
                    #'architecture.builder-protocol:walk-nodes
                    builder atom-type object))))
            (ensure-same (replace-timestamps calls timestamp-replacements)
                         expected-calls
                         :test #'equal)))
        cases))

(addtest (rsbag-builder-root
          :documentation
          "Smoke test for the `transport-options'.")
  bag/smoke

  (let* ((start        (local-time:now))
         (start/uint64 (rsbag.backend:timestamp->uint64 start))
         (end          (local-time:adjust-timestamp start
                         (:offset :sec 1)))
         (end/uint64   (rsbag.backend:timestamp->uint64 end))
         (content `(((,start/uint64 ,end/uint64) ("a" "b") 0 "foo" ()))))
    (with-mock-bag (bag-1 :direction :input) '()
      (with-mock-bag (bag-2 :direction :input) content
        (let ((channel (first (bag-channels bag-2))))
          (check-un-build-calls
           t 'string
           `((,start/uint64 . ,start) (,end/uint64 . ,end))
           `((,bag-1 ((:peek  nil () ,bag-1)
                      (:visit nil () ,bag-1 rsbag:bag ((:channel . (:map . :name)))
                              (:location nil :event-count 0))))
             (,bag-2 ((:peek  nil () ,bag-2)
                      (:visit nil () ,bag-2 rsbag:bag ((:channel . (:map . :name)))
                              (:location nil
                                         :event-count 2
                                         :start       ,start/uint64
                                         :end         ,end/uint64
                                         :duration    1
                                         :rate        2))
                      (:peek  :channel (:name "foo") ,channel)
                      (:visit :channel (:name "foo") ,channel rsbag:channel ()
                              (:name        "foo"
                                            :event-count 2
                                            :start       ,start/uint64
                                            :end         ,end/uint64
                                            :duration    1
                                            :rate        2))))))

          (check-un-build-calls
           (make-instance 'rsbag.builder:unbuilder :compute-sizes? t) 'string
           `((,start/uint64 . ,start) (,end/uint64 . ,end))
           `((,bag-1 ((:peek  nil () ,bag-1)
                      (:visit nil () ,bag-1 rsbag:bag ((:channel . (:map . :name)))
                              (:location nil :data-size 0 :event-count 0))))
             (,bag-2 ((:peek  nil () ,bag-2)
                      (:visit nil () ,bag-2 rsbag:bag ((:channel . (:map . :name)))
                              (:location    nil
                                            :data-size   2
                                            :event-count 2
                                            :start       ,start/uint64
                                            :end         ,end/uint64
                                            :duration    1
                                            :rate        2))
                      (:peek  :channel (:name "foo") ,channel)
                      (:visit :channel (:name "foo") ,channel rsbag:channel ()
                              (:name        "foo"
                                            :data-size   2
                                            :event-count 2
                                            :start       ,start/uint64
                                            :end         ,end/uint64
                                            :duration    1
                                            :rate        2)))))))))))

(addtest (rsbag-builder-root
          :documentation
          "Smoke test for the `transport-options'.")
  channel/smoke

  (let* ((start        (local-time:now))
         (start/uint64 (rsbag.backend:timestamp->uint64 start))
         (end          (local-time:adjust-timestamp start
                         (:offset :sec 1)))
         (end/uint64   (rsbag.backend:timestamp->uint64 end))
         (type         '(:rsb-event-0.9 :foo))
         (format       "format")
         (content      `(((,start/uint64 ,end/uint64)
                          ("a" "b") 0 "foo"
                          (:type ,type :format ,format)))))
    (with-mock-bag (bag :direction :input :transform '(nil)) content
      (let ((channel (first (bag-channels bag))))
        (check-un-build-calls
         t '(or string cons)
         `((,start/uint64 . ,start) (,end/uint64 . ,end))
         `((,channel ((:peek  nil () ,channel)
                      (:visit nil () ,channel rsbag:channel
                              ((:type . 1))
                              (:name        "foo"
                               :event-count 2
                               :start       ,start/uint64
                               :end         ,end/uint64
                               :duration    1
                               :rate        2))
                      (:peek  :type () ,type)))))

        (check-un-build-calls
         (make-instance 'rsbag.builder::unbuilder
                        :format? t)
         '(or string cons)
         `((,start/uint64 . ,start) (,end/uint64 . ,end))
         `((,channel ((:peek  nil     () ,channel)
                      (:visit nil     () ,channel rsbag:channel
                              ((:type   . 1)
                               (:format . 1))
                              (:name        "foo"
                               :event-count 2
                               :start       ,start/uint64
                               :end         ,end/uint64
                               :duration    1
                               :rate        2))
                      (:peek  :type   () ,type)
                      (:peek  :format () ,format)))))

        (check-un-build-calls
         (make-instance 'rsbag.builder::unbuilder
                        :compute-sizes? t)
         '(or string cons)
         `((,start/uint64 . ,start) (,end/uint64 . ,end))
         `((,channel ((:peek  nil     () ,channel)
                      (:visit nil     () ,channel rsbag:channel
                              ((:type   . 1))
                              (:name        "foo"
                               :data-size   2
                               :event-count 2
                               :start       ,start/uint64
                               :end         ,end/uint64
                               :duration    1
                               :rate        2))
                      (:peek  :type   () ,type)))))))))
