;;;; rsbag-tidelog.asd --- System definition for TIDELog backend of rsbag.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag-tidelog-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsbag-tidelog-system)

#.(progn
    (load (merge-pathnames "cl-rsbag.asd" *load-truename*))
    (values))

(asdf:defsystem "rsbag-tidelog"
  :description "TIDE log file format backend for cl-rsbag."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(cl-rsbag-system:version/string)
  :depends-on  ((:version "cl-rsbag" #.(cl-rsbag-system:version/string)))

  :components  ((:module     "tidelog"
                 :pathname   "src/backend/tidelog"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "variables")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "util")

                              (:file       "generator")
                              (:file       "macros")

                              (:file       "spec")
                              (:file       "io")

                              (:file       "index-vector")
                              (:file       "index")
                              (:file       "file")

                              (:file       "repair"))))

  :in-order-to ((test-op (test-op "rsbag-tidelog/test"))))

(asdf:defsystem "rsbag-tidelog/test"
  :description "Unit tests for the rsbag-tidelog system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(cl-rsbag-system:version/string)
  :depends-on  ((:version "lift"          "1.7.1")

                (:version "rsbag-tidelog" #.(cl-rsbag-system:version/string))

                (:version "cl-rsbag/test" #.(cl-rsbag-system:version/string)))

  :components  ((:module     "tidelog"
                 :pathname   "test/backend/tidelog"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "io")

                              (:file       "file")

                              (:file       "repair"))))

  :perform     (test-op (operation component)
                 (funcall (find-symbol "RUN-TESTS" :lift)
                          :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                                           "lift-tidelog.config"))))
