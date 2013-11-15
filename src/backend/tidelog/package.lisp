;;;; package.lisp --- Package definition for the backend.tidelog module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.tidelog
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsbag
   #:rsbag.backend)

  ;; Conditions
  (:export
   #:tidelog-condition

   #:tidelog-file-error

   #:invalid-tidelog-structure

   #:no-such-block-class-error
   #:no-such-block-class-error-tag)

  (:documentation
   "This package contains a backend for the TIDE log file format as
    specified at https://retf.info/svn/drafts/rd-0001.txt.

    The most important interface classes are:

    file                                [class]

      Represents an entire TIDE log file (and handles access to its
      channels).

    index                               [class]

      Represents the index for one channel of a TIDE log file.

    In addition, there is lower-level input/output machinery based on
    code generated according to the TIDE log specification. This level
    operates on TIDE log blocks.

    The interface consists of the generic functions

    tag                                 [generic function]

      Returns the four-character \"tag-string\" identifying the block
      type of an object or class.

    size                                [generic function]

      Returns the number of octets required to store a block object.

    scan                                [generic function]

      Finds blocks in a TIDE log file without reading their entire
      contents.

    unpack                              [generic function]

      Which reads blocks from TIDE log files.

    pack                                [generic function]

      Which stores blocks in TIDE log files

    The code generation is implemented by

    spec->*                             [function]

      Family of functions for generating methods on the above generic
      functions for specified block types.

    define-element                      [macro]

      Turns declarative block type specifications into calls to
      spec->* functions."))
