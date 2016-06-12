;;;; builder.lisp --- (un-)build protocol for bags and channels.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.builder
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:architecture.builder-protocol

   #:rsbag)

  (:export
   #:unbuilder)

  (:documentation
   "Support of the (un-)build protocol for rsbag objects."))

(cl:in-package #:rsbag.builder)

;;; Customizable unbuilder

(defclass unbuilder (print-items:print-items-mixin)
  ((initarg-if-missing :initarg :initarg-if-missing
                       :reader   unbuilder-initarg-if-missing
                       :initform :omit
                       :documentation
                       "A value that should be used in place of
                        missing initarg values or the symbol :omit to
                        omit the initargs entirely.")
   (compute-sizes?     :initarg :compute-sizes?
                       :reader   unbuilder-compute-sizes?
                       :initform nil
                       :documentation
                       "Should the size in octets of the data
                        contained in each channel be computed?")
   (format?            :initarg  :format?
                       :reader   unbuilder-format?
                       :initform nil
                       :documentation
                       "Should the format information associated to
                        channels be traversed?"))
  (:documentation
   "Specialized builder for providing information about `bag' and
    `channel' instances."))

(defmethod print-items:print-items append ((object unbuilder))
  (let+ (((&structure-r/o
           unbuilder- initarg-if-missing compute-sizes? format?)
          object))
    `((:initarg-if-missing ,initarg-if-missing "~S")
      (:compute-sizes?     ,compute-sizes?     "~@[ sizes~]"
       ((:after :initarg-if-missing)))
      (:format?            ,format?            "~@[ format ~S~]"
       ((:after :compute-sizes?))))))

;;; Bag

(defmethod node-kind ((builder unbuilder) (node bag))
  'rsbag:bag)

(defmethod node-initargs ((builder unbuilder) (node bag))
  (let+ (((&structure-r/o unbuilder- initarg-if-missing) builder)
         ((&structure-r/o bag- location channels) node)
         (count (reduce #'+ channels :key #'length)))
    (list* :location location
           (count-and-timing-initargs
            node count :if-missing initarg-if-missing))))

(defmethod node-relations ((builder unbuilder) (node bag))
  '((:channel . (:map . :name))))

(defmethod node-relation ((builder  unbuilder)
                          (relation (eql :channel))
                          (node     bag))
  (let ((channels (bag-channels node)))
    (values channels (mapcar (lambda (channel)
                               (list :name (channel-name channel)))
                             channels))))

;;; Channel

(defmethod node-kind ((builder unbuilder) (node channel))
  'rsbag:channel)

(defmethod node-initargs ((builder unbuilder) (node channel))
  (let+ (((&structure-r/o unbuilder- initarg-if-missing compute-sizes?)
          builder))
    (list* :name (channel-name node)
           (append
            (when compute-sizes?
              (list :data-size (reduce #'+ node :key #'length)))
            (count-and-timing-initargs
             node (length node) :if-missing initarg-if-missing)))))

(defmethod node-relations ((builder unbuilder)
                           (node    channel))
  (let+ (((&structure-r/o unbuilder- format?) builder)
         (meta-data (channel-meta-data node)))
    `(,@(when (getf meta-data :type)
          '((:type . 1)))
      ,@(when (and format? (getf meta-data :format))
          '((:format . 1))))))

(defmethod node-relation ((builder  unbuilder)
                          (relation (eql :type))
                          (node     channel))
  (getf (channel-meta-data node) :type))

(defmethod node-relation ((builder  unbuilder)
                          (relation (eql :format))
                          (node     channel))
  (getf (channel-meta-data node) :format))

;;; Default methods

(defvar *default-unbuilder* (make-instance 'unbuilder))

(macrolet ((define-delegating-methods (class)
             `(progn
                (defmethod node-kind ((builder t) (node ,class))
                  (node-kind *default-unbuilder* node))

                (defmethod node-initargs ((builder t) (node ,class))
                  (node-initargs *default-unbuilder* node))

                (defmethod node-relations ((builder t) (node ,class))
                  (node-relations *default-unbuilder* node))

                (defmethod node-relation ((builder  t)
                                          (relation t)
                                          (node     ,class))
                  (node-relation *default-unbuilder* relation node)))))
  (define-delegating-methods bag)
  (define-delegating-methods channel))

;;; Utilities

(defun count-and-timing-initargs (thing count &key if-missing)
  (let+ (((&accessors-r/o (start start-timestamp) (end end-timestamp)) thing)
         (emit-missing? (not (eq if-missing :omit)))
         (duration      (when (and start end)
                          (local-time:timestamp-difference end start)))
         (rate          (when (and duration (plusp duration))
                          (/ count duration))))
    (append (list :event-count count)
            (when (or start emit-missing?)
              (list :start (or start if-missing)))
            (when (or end emit-missing?)
              (list :end (or end if-missing)))
            (when (or duration emit-missing?)
              (list :duration (or duration if-missing)))
            (when (or rate emit-missing?)
              (list :rate (or rate if-missing))))))
