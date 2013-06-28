;;;; rsbag-tidelog.asd --- System definition for TIDELog backend of rsbag.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(defsystem :rsbag-tidelog
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsbag-system:version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "TIDE log file format backend for cl-rsbag."
  :depends-on  ((:version :cl-rsbag #.(cl-rsbag-system:version/string)))
  :components  ((:module     "tidelog"
                 :pathname   "src/backend/tidelog"
                 :components ((:file       "package")
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "variables"
                               :depends-on ("package"))
                              (:file       "util"
                               :depends-on ("package"))

                              (:file       "generator"
                               :depends-on ("package"))
                              (:file       "macros"
                               :depends-on ("package" "generator"))

                              (:file       "spec"
                               :depends-on ("package" "macros"))
                              (:file       "io"
                               :depends-on ("package" "conditions"
                                                      "util" "spec"))

                              (:file       "index"
                               :depends-on ("package" "spec" "io"))
                              (:file       "file"
                               :depends-on ("package" "variables"
                                                      "spec" "io"))

                              (:file       "repair"
                               :depends-on ("package" "spec" "io"))))))
