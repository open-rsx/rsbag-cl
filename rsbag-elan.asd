;;;; rsbag-elan.asd --- System definition for ELAN backend of rsbag.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag-elan-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsbag-elan-system)

#.(progn
    (load (merge-pathnames "rsbag.asd" *load-truename*))
    (values))

(asdf:defsystem "rsbag-elan"
  :description "Elan file format backend for rsbag."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(rsbag-system:version/string)
  :depends-on  ((:version "xml.location"                "0.2.0")
                (:version "xml.location-and-local-time" "0.2.0")

                (:version "rsbag"                       #.(rsbag-system:version/string)))

  :components  ((:module     "elan"
                 :pathname   "src/backend/elan"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "util")
                              (:file       "variables")
                              (:file       "xml")
                              (:file       "file"))))

  :in-order-to ((test-op (test-op "rsbag-elan/test"))))

(asdf:defsystem "rsbag-elan/test"
  :description "Unit tests for the rsbag-elan system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(rsbag-system:version/string)
  :depends-on  ((:version "lift"       "1.7.1")

                (:version "rsbag-elan" #.(rsbag-system:version/string))

                (:version "rsbag/test" #.(rsbag-system:version/string)))

  :components  ((:module     "elan"
                 :pathname   "test/backend/elan"
                 :serial     t
                 :components ((:file       "package"))))

  :perform     (test-op (operation component)
                 (funcall (find-symbol "RUN-TESTS" :lift)
                          :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                                           "lift-elan.config"))))
