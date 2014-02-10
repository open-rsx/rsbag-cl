;;;; package.lisp --- Package definition for the rsb.replay module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.rsb.replay
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsbag
   #:rsbag.transform
   #:rsbag.view
   #:rsbag.rsb

   #:rsb
   #:rsb.patterns.request-reply)

  (:shadowing-import-from #:rsbag
   #:direction

   #:meta-data
   #:meta-data-count
   #:meta-data-keys
   #:meta-data-values
   #:meta-data-plist
   #:meta-data-alist)

  ;; Types
  (:export
   #:range-boundary/timestamp)

  ;; `error-policy-mixin' mixin class
  (:export
   #:error-policy-mixin)

  ;; index bounds protocol and mixin class
  (:export
   #:strategy-start-index
   #:strategy-end-index

   #:bounds-mixin)

  ;; time bounds protocol and mixin class
  (:export
   #:strategy-start-time
   #:strategy-end-time

   #:time-bounds-mixin)

  ;; view creation protocol and mixin class
  (:export
   #:make-view

   #:view-creation-mixin)

  ;; sequential processing protocol and mixin class
  (:export
   #:sequential-mixin)

  ;; speed adjustment protocol and mixin class
  (:export
   #:strategy-speed

   #:speed-adjustment-mixin)

  ;; delay limiting protocol and mixin class
  (:export
   #:strategy-max-delay

   #:delay-limiting-mixin)

  ;; external driver protocol and mixin class
  (:export
   #:make-commands
   #:strategy-commands
   #:find-command
   #:next-command
   #:execute-command

   #:external-driver-mixin)

  ;; `delay-correcting-mixin' mixin class
  (:export
   #:delay-correcting-mixin

   #:strategy-previous-delay
   #:strategy-previous-call)

  ;; `timestamp-adjustment-mixin' mixin class
  (:export
   #:timestamp-adjustment-mixin

   #:strategy-adjustments)

  ;; `recorded-timing' replay strategy class
  (:export
   #:recorded-timing)

  ;; `fixed-rate' replay strategy class
  (:export
   #:fixed-rate

   #:strategy-rate
   #:strategy-delay)

  ;; `as-fast-as-possible' replay strategy class
  (:export
   #:as-fast-as-possible)

  ;; `remote-controlled' replay strategy class
  (:export
   #:remote-controlled)

  ;; `interactive' replay strategy class
  (:export
   #:interactive

   #:strategy-stream
   #:strategy-previous-command)

  (:documentation
   "This package contains supporting infrastructure and replay
    strategy classes."))
