;;;; protocol.lisp --- Protocol functions of the backend.tidelog module.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

;;; Block IO protocol
;;;
;;; This protocol allows `pack' ing block instances into destinations
;;; (e.g. streams, `octet-vector' s), `unpack' ing block instances
;;; from sources and `scan' ning sources for blocks.

(defgeneric tag (object-or-class)
  (:documentation
   "Return the tag for the block OBJECT-OR-CLASS as `octet-vector'."))

(defgeneric size (object)
  (:documentation
   "Return the packed size in bytes of OBJECT."))

(defgeneric scan (source object &optional start)
  (:documentation
   "Scan SOURCE for OBJECT and return the collected instances.

    In the most important case, OBJECT is :tide which causes SOURCE to
    be scanned as an entire TIDELog file. In this case, three values
    are returned:
    1. a list of CHAN blocks
    2. a list of INDX blocks
    3. a list of descriptors of CHNK blocks. Each descriptor is of the
       form (ID . OFFSET)

    If corrupt data is detected, an `invalid-tidelog-structure' is
    signaled.

    The following restarts are established:

    `retry'

      Retry processing a particular block.

    `continue'

      Skip the current block and continue with the next block if
      possible or stop processing and return otherwise.

    `abort'

      Stop processing and return the blocks collected so far."))

(defgeneric unpack (source object &optional start)
  (:documentation
   "Unpack OBJECT from SOURCE, optionally starting at offset START.

    OBJECT can at least be one of the following

    :block-header

      Extract a block header from SOURCE and return two values: the
      class of the block and the length of the block in bytes. If the
      block tag in SOURCE does not correspond to a known block
      class, a `no-such-block-class-error' is signaled.

    :block

      Detect the block class and unpack the data in SOURCE into an
      appropriate instance. Can signal the same errors as
      the :block-header case when determining the block class.

    INSTANCE-OF-A-BLOCK-CLASS

      Unpack the data in SOURCE into the given block class
      instance. The data in SOURCE should start with the block body,
      not the block header in this case.

    If corrupt data is detected, an `invalid-tidelog-structure' is
    signaled.

    The following  "))

(defgeneric pack (object destination &optional start)
  (:documentation
   "Pack the block class instance OBJECT into DESTINATION."))

;;; Index protocol
;;;
;;; Generic functions for translating entry indices and entry
;;; timestamps to file offsets.

(defgeneric index-count (index)
  (:documentation
   "Return the number of entries stored in INDEX."))

(defgeneric index-index->offset (index index*)
  (:documentation
   "Return the offset of the entry designated by INDEX* in INDEX."))

(defgeneric index-timestamp->offset (index timestamp)
  (:documentation
   "Return the offset of the entry designated by TIMESTAMP in INDEX."))

(defgeneric index-add-indxs (index indxs chunks)
  (:documentation
   "Add INDXS to INDEX, using CHUNKS to compute file offsets."))

(defgeneric index-add-entries (index entries chunks)
  (:documentation
   "Add ENTRIES to INDEX, using CHUNKS to compute file offsets."))

;;; Output index protocol
;;;
;;; Flush management of output indices.

(defgeneric index-derive-flush-strategy (index flush-strategy)
  (:documentation
   "Return a flush-strategy for INDEX based on FLUSH-STRATEGY."))
