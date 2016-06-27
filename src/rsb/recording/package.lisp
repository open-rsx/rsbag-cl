;;;; package.lisp --- Package definition for the rsb.recording module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.rsb.recording
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rsbag
   #:rsbag.rsb)

  ;; Conditions
  (:export
   #:recording-error

   #:entry-storage-error)

  ;; Processing protocol
  (:export
   #:process-event)

  ;; Channel allocation strategy protocol
  (:export
   #:channel-name-for
   #:channel-transform-for
   #:channel-format-for
   #:channel-meta-data-for

   #:make-channel-for
   #:ensure-channel-for

   #:strategy ; service

   #:make-strategy)

  (:documentation
   "This package contains supporting infrastructure and channel
    allocation strategy classes."))
