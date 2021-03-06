;;;; conditions.lisp --- Conditions used in the transform module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.transform)

(define-condition transform-condition (rsbag-condition)
  ((transform :initarg  :transform
              :reader   transform-condition-transform
              :documentation
              "Stores the transform instance that was used in the
               failed transform operation."))
  (:default-initargs
   :transform (missing-required-initarg 'transform-condition :transform))
  (:documentation
   "This condition class can be mixed into condition classes which
    have an associated transform."))

(define-condition encoding-error (rsbag-error
                                  transform-condition
                                  chainable-condition)
  ((domain-object :initarg  :domain-object
                  :reader   transform-error-domain-object
                  :documentation
                  "Stores the domain object the encoding of which
                   failed."))
  (:default-initargs
   :domain-object (missing-required-initarg 'encoding-error :domain-object))
  (:report
   (lambda (condition stream)
     (let ((*print-length* (or *print-length* 16)))
       (format stream "~@<The domain object ~S could not be encoded by ~
                       the transform ~
                       ~A.~/more-conditions:maybe-print-cause/~@:>"
               (transform-error-domain-object condition)
               (transform-condition-transform condition)
               condition))))
  (:documentation
   "This error is signaled when the encoding of a domain object for
    storage in bag fails."))

(define-condition decoding-error (rsbag-error
                                  transform-condition
                                  chainable-condition)
  ((encoded :initarg  :encoded
            :reader   transform-error-encoded
            :documentation
            "Stores the encoded data, the decoding of which failed."))
  (:default-initargs
   :encoded (missing-required-initarg 'decoding-error :encoded))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o (encoded   transform-error-encoded)
                             (transform transform-condition-transform)) condition)
            (octet-sequence? (and (typep encoded 'sequence)
                                  (every (of-type 'octet) encoded))))
       ;; colon -> print offset
       ;; at    -> print header
       (format stream "~@<The encoded value ~:@_~:@_~
                       ~<| ~@;~
                         ~:[~
                           ~S~
                         ~;~
                           ~,,,16:@/utilities.binary-dump:print-binary-dump/~
                         ~]~
                       ~:>~:@_~:@_~
                       could not be decoded by the transform ~:_~
                       ~A.~/more-conditions:maybe-print-cause/~:>"
               (list octet-sequence? encoded) transform condition))))
  (:documentation
   "This error is signaled when the decoding of data, usually
    retrieved from a bag, fails."))
