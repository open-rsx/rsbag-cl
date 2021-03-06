;;;; package.lisp --- Package definition for the transform module.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.transform
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:nibbles)

  (:import-from #:rsbag
   #:rsbag-condition
   #:rsbag-error

   #:make-versioned-name
   #:with-versioned-packages)

  ;; Variables
  (:export
   #:+rsb-schema-name+)

  ;; Conditions
  (:export
   #:transform-condition
   #:transform-condition-transform

   #:encoding-error
   #:transform-error-domain-object

   #:decoding-error
   #:transform-error-encoded)

  ;; Transform protocol
  (:export
   #:transform-name
   #:transform-format

   #:decode
   #:encode)

  ;; Transform service and creation protocol
  (:export
   #:transform

   #:make-transform)

  (:documentation
   "This package contains the transformation protocol and
    infrastructure used in rsbag."))
