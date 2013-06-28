;;;; rsbag-elan.asd --- System definition for ELAN backend of rsbag.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag-elan-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsbag-elan-system)

#.(progn
    (load (merge-pathnames "cl-rsbag.asd" *load-truename*))
    (values))

(defsystem :rsbag-elan
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsbag-system:version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "Elan file format backend for cl-rsbag."
  :depends-on  ((:version :xml.location                "0.2.0")
                (:version :xml.location-and-local-time "0.2.0")

                (:version :cl-rsbag                    #.(cl-rsbag-system:version/string)))
  :components  ((:module     "elan"
                 :pathname   "src/backend/elan"
                 :components ((:file       "package")
                              (:file       "types"
                               :depends-on ("package"))
                              (:file       "xml"
                               :depends-on ("package" "types"))
                              (:file       "file"
                               :depends-on ("package" "types" "xml"))))))
