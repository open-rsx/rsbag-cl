;;;; package.lisp --- Package definition for view module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.view
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:rsbag)

  ;; `multi-sequence-view-mixin'
  (:export
   #:view-sequences)

  ;; `serialized' view class and construction function
  (:export
   #:serialized

   #:make-serialized-view

   #:%make-key-function)

  (:documentation
   "This module contains functions and classes that implement
    views (as subclasses of `cl:sequence') on data stored in
    bags. Currently, the following views are available:

    serialized

      aggregate the data of multiple channels, serializing items using
      their timestamps."))
