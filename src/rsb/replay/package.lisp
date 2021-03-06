;;;; package.lisp --- Package definition for the rsb.replay module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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
   #:rsbag.rsb)

  ;; Types
  (:export
   #:range-boundary/timestamp

   #:event-id-adjustment)

  ;; Conditions
  (:export
   #:replay-error
   #:replay-error-strategy

   #:entry-retrieval-error

   #:entry-processing-error)

  ;; Replay protocol
  (:export
   #:replay)

  ;; Sequential processing protocol
  (:export
   #:process-event)

  ;; Timed replay protocol
  (:export
   #:schedule-event)

  ;; Replay bag connection protocol
  (:export
   #:connection-strategy

   #:replay-bag-connection)

  ;; Service and strategy creation protocol
  (:export
   #:strategy                       ; service

   #:make-strategy)

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

  ;; repetitions mixin class
  (:export
   #:repetitions-mixin)

  ;; view creation protocol and mixin class
  (:export
   #:make-view

   #:view-creation-mixin)

  ;; filtering protocol and mixin class
  (:export
   #:strategy-filter

   #:filtering-mixin)

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

  ;; `event-id-mixin' mixin class
  (:export
   #:event-id-mixin

   #:strategy-event-id)

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
