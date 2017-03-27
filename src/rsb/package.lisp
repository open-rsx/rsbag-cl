;;;; package.lisp --- Package definition for rsb module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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
   #:entry-condition-entry)

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

  ;; Composite connection protocol and `composite-connection-mixin' class
  (:export
   #:connection-direct-connections
   #:connection-connections

   #:composite-connection-mixin)

  ;; `bag-connection' class and protocol
  (:export
   #:bag-connection

   #:connection-bag
   #:connection-channels)

  ;; `recording-bag-connection' class
  (:export
   #:recording-bag-connection)

  ;; `replay-bag-connection' subclass and protocol
  (:export
   #:replay-bag-connection

   #:connection-strategy)

  ;; `channel-connection' class and protocol
  (:export
   #:channel-connection)

  ;; `endpoint-channel-connection' class and protocol
  (:export
   #:endpoint-channel-connection

   #:connection-endpoint)

  ;; `participant-channel-connection' subclass
  (:export
   #:participant-channel-connection)

  (:documentation
   "This package contains functions and classes that enable the
    recording and playback of RSB events into/from rsbag log files."))
