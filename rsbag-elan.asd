;;;; rsbag-elan.asd --- System definition for ELAN backend of rsbag.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Elan file format backend for cl-rsbag."
  :depends-on  ((:version :xml.location                "0.2.0")
                (:version :xml.location-and-local-time "0.2.0")

                (:version :cl-rsbag                    #.(cl-rsbag-system:version/string)))
  :components  ((:module     "elan"
                 :pathname   "src/backend/elan"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "util")
                              (:file       "variables")
                              (:file       "xml")
                              (:file       "file"))))

  :in-order-to ((test-op (test-op :rsbag-elan/test))))

(defsystem :rsbag-elan/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsbag-system:version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsbag-elan system."
  :depends-on  ((:version :lift          "1.7.1")

                (:version :rsbag-elan    #.(cl-rsbag-system:version/string))

                (:version :cl-rsbag/test #.(cl-rsbag-system:version/string)))
  :components  ((:module     "elan"
                 :pathname   "test/backend/elan"
                             :serial     t
                             :components ((:file       "package")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsbag-elan/test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
           :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                            "lift-elan.config")))
