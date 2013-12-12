;;;; package.lisp --- Package definition for rsb module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.rsb
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsbag
   #:rsbag.transform
   #:rsbag.view

   #:rsb)

  (:shadowing-import-from #:rsbag
   #:direction

   #:meta-data
   #:meta-data-count
   #:meta-data-keys
   #:meta-data-values
   #:meta-data-plist
   #:meta-data-alist)

  ;; Conditions
  (:export
   #:connection-condition
   #:connection-condition-connection

   #:entry-condition
   #:entry-condition-entry

   #:recording-error

   #:entry-storage-error

   #:replay-error
   #:replay-error-strategy

   #:entry-retrieval-error

   #:entry-processing-error)

  ;; connection construction protocol
  (:export
   #:events->bag
   #:bag->events)

  ;; connection protocol
  (:export
   #:done?
   #:wait

   #:start
   #:stop)

  ;; Convenience macros
  (:export
   #:with-open-connection
   #:with-events->bag
   #:with-bag->events)

  ;; `bag-connection' class and protocol
  (:export
   #:bag-connection

   #:connection-bag
   #:connection-channels)

  ;; `replay-bag-connection' subclass and protocol
  (:export
   #:replay-bag-connection

   #:connection-strategy)

  ;; `channel-connection' class and protocol
  (:export
   #:channel-connection

   #:connection-endpoint)

  ;; `participant-channel-connection' subclass
  (:export
   #:participant-channel-connection)

  ;; channel allocation strategy protocol
  (:export
   #:channel-name-for
   #:channel-format-for
   #:make-channel-for

   #:no-such-channel-strategy-class
   #:find-channel-strategy-class
   #:channel-strategy-classes

   #:make-channel-strategy)

  ;; replay strategy protocol
  (:export
   #:replay

   #:process-event
   #:schedule-event

   #:no-such-replay-strategy-class
   #:find-replay-strategy-class
   #:replay-strategy-classes

   #:make-replay-strategy)

  (:documentation
   "This package contains functions and classes that enable the
    recording and playback of RSB events into/from rsbag log files."))
