;;;; package.lisp --- Package definition for backend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsbag.backend
  (:use
   :cl
   :alexandria
   :let-plus
   :more-conditions

   :rsbag)

  (:import-from :closer-mop
   :generic-function-methods
   :method-specializers
   :eql-specializer
   :eql-specializer-object)

  ;; Conditions
  (:export
   :log-file-error
   :log-file-error-source

   :invalid-file-structure)

  ;; backend protocol
  (:export
   :backend-location
   :backend-direction

   :get-channels
   :make-channel-id
   :put-channel

   :get-num-entries
   :get-timestamps

   :get-entry
   :put-entry)

  ;; Backend findable class family
  (:export
   :no-such-backend-class
   :find-backend-class
   :backend-classes)

  ;; `stream-mixin' class
  (:export
   :stream-mixin

   :backend-stream)

  ;; `direction-mixin' class
  (:export
   :direction-mixin)

  ;; `location-mixin' class
  (:export
   :location-mixin)

  ;; `buffering-writer-mixin' class and protocol
  (:export
   :buffering-writer-mixin

   :buffer-property

   :backend-buffer
   :make-buffer
   :write-buffer
   :flush

   :backend-flush-strategy)

  ;; `async-double-buffered-writer-mixin'
  (:export
   :async-double-buffered-writer-mixin)

  ;; `last-write-time-mixin'
  (:export
   :last-write-time-mixin)

  ;; Flush strategy protocol
  (:export
   :flush?)

  ;; Flush strategy class family
  (:export
   :no-such-flush-strategy-class
   :find-flush-strategy-class
   :flush-strategy-classes

   :make-flush-strategy)

  (:documentation
   "This package contains protocol and implementation aids for file
format backends for cl-rsbag."))
