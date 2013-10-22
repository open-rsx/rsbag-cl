;;;; package.lisp --- Package definition for the backend.elan module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.backend.elan
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate

   #:rsbag
   #:rsbag.backend)

  (:documentation
   "This package contains a reading and writing backend for the
    XML-based EAF file format used by the Elan annotation software."))
