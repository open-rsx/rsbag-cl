;;;; protocol.lisp --- Backend protocol of the cl-rsbag system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend)

;;; Backend protocol

(defgeneric backend-location (backend)
  (:documentation
   "Return an object representing the location in which the contents
of BACKEND is stored. Return NIL if this information is not
available."))

(defgeneric backend-direction (backend)
  (:documentation
   "Return the direction for which BACKEND is configured. See type
`rsbag:direction'."))

(defgeneric get-channels (backend)
  (:documentation
   "Return the list of channels that are stored in the data source
represented by BACKEND. Elements of the list are of the form (ID NAME
META-DATA)."))

(defgeneric make-channel-id (backend name)
  (:documentation
   "Return an object suitable for uniquely identifying a channel
within BACKEND. The object may be based on NAME."))

(defgeneric put-channel (backend channel name meta-data)
  (:documentation
   "Add a channel name NAME identified by CHANNEL with meta-data
META-DATA to the list of channels that are stored in the data source
represented by BACKEND."))

(defgeneric get-num-entries (backend channel)
  (:documentation
   "Return the number of entries stored in CHANNEL of the data source
represented by BACKEND."))

(defgeneric get-timestamps (backend channel)
  (:documentation
   "Return a list of the timestamps for which entries are stored in
CHANNEL of the data source represented by BACKEND."))

(defgeneric get-entry (backend channel index)
  (:documentation
   "Retrieve and return the entry designated by INDEX of CHANNEL in
the data source represented by BACKEND."))

(defgeneric put-entry (backend channel index entry)
  (:documentation
   "Store ENTRY at the position designated by INDEX in CHANNEL in the
data source represented by BACKEND."))

;;; Stream protocol

(defgeneric backend-stream (backend)
  (:documentation
   "Return the stream of the data source of BACKEND."))

;;; Buffering protocol

(defgeneric buffer-property (backend buffer name)
  (:documentation
   "Return the property of BUFFER used in BACKEND designated by
NAME."))

(defgeneric make-buffer (backend previous)
  (:documentation
   "Allocate and return a suitable buffer for BACKEND based on the
buffer PREVIOUS. PREVIOUS can be nil, for the initial allocation."))

(defgeneric write-buffer (backend buffer)
  (:documentation
   "Write the entries accumulated in BUFFER to the data source
represented by BACKEND."))

(defgeneric flush (backend buffer)
  (:documentation
   "Commit the entries accumulated in BUFFER to the data source
represented by BACKEND."))

(defgeneric backend-flush-strategy (backend)
  (:documentation
   "Return the strategy which is used by backend to schedule buffer
flushes."))

;;; Finding backend classes

(dynamic-classes:define-findable-class-family backend
    "This class family consists of file format backends.")

;;; Flush strategy protocol

(defgeneric flush? (strategy backend buffer)
  (:documentation
   "Return non-nil when BUFFER which is used by BACKEND should be
flushed according to STRATEGY."))

;;; Flush strategy classes

(dynamic-classes:define-findable-class-family flush-strategy
    "This class family consist of flush strategy classes which
implement strategies for flushing backend buffers when writing log
files.")

(defgeneric make-flush-strategy (thing &rest args)
  (:documentation
   "Return (potentially creating it first) an instance of the flushing
strategy designated by THING."))

(defmethod make-flush-strategy ((thing symbol) &rest args)
  (apply #'make-flush-strategy
         (if (keywordp thing)
             (find-flush-strategy-class thing)
             (find-class thing))
         args))

(defmethod make-flush-strategy ((thing class) &rest args)
  (apply #'make-instance thing args))
