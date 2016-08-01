;;;; package.lisp --- Package definition for the cl-rsbag system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions)

  ;; Symbols
  (:export
   #:&from-source)

  ;; Types
  (:export
   #:direction

   #:if-does-not-exist-policy
   #:if-exists-policy

   #:transform-spec/default
   #:transform-spec/augment
   #:transform-spec/full
   #:transform-spec)

  ;; Conditions
  (:export
   #:rsbag-condition
   #:rsbag-problem-condition
   #:rsbag-error

   #:open-error
   #:open-error-source

   #:bag-condition
   #:bag-condition-bag

   #:no-such-channel
   #:no-such-channel-name

   #:direction-error
   #:direction-error-expected-direction

   #:channel-condition
   #:channel-condition-channel

   #:channel-open-error

   #:channel-exists

   #:no-such-entry
   #:no-such-entry-key)

  ;; meta-data protocol
  (:export
   #:meta-data
   #:meta-data-count
   #:meta-data-keys
   #:meta-data-values
   #:meta-data-plist
   #:meta-data-alist)

  ;; `bag' class and file protocol
  (:export
   #:bag

   #:bag-location
   #:bag-direction
   #:bag-transform
   #:bag-channels
   #:bag-channel

   #:create ; restart

   #:open-bag)

  ;; `synchronized-channel' class
  (:export
   #:synchronized-channel)

  ;; `synchronized-bag' class
  (:export
   #:synchronized-bag)

  ;; `channel' class and channel protocol
  (:export
   #:channel

   #:channel-bag
   #:channel-name
   #:channel-meta-data
   #:channel-transform

   #:channel-%id
   #:channel-%backend

   #:channel-timestamps
   #:channel-entries
   #:channel-items

   #:entry)

  ;; Time range protocol
  (:export
   #:start-timestamp
   #:end-timestamp)

  ;; Convenience macros
  (:export
   #:call-with-open-bag
   #:with-open-bag

   #:with-bag)

  ;; Package management macros
  (:export
   #:make-versioned-name
   #:with-renamed-package
   #:with-renamed-packages
   #:with-versioned-packages)

  ;; Threadpool
  (:export
   #:start-threadpool
   #:stop-threadpool
   #:enable-restart-threadpool

   #:with-threadpool)

  ;; Error handling utilities
  (:export
   #:function-calling-restart-bind)

  ;; Print utilities
  (:export
   #:print-direction
   #:print-location)

  (:documentation
   "This package contains the Common Lisp implementation of RSBag.

    The client interface primarily consists of the `bag' and `channel'
    classes. Conceptually, bags consists of multiple named channels
    which in turn contain sequences of timestamped data items."))
