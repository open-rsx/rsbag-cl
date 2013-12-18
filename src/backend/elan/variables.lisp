;;;; variables.lisp --- Variables used by the Elan backend.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.elan)

(defparameter +schema-url+
  (puri:uri "http://www.mpi.nl/tools/elan/EAFv2.7.xsd"))

(defparameter +format-version-major+ 2
  "Major version of the ELAN format.")

(defparameter +format-version-minor+ 2
  "Minor version of the ELAN format.")
