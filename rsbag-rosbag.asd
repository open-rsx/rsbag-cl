;;;; rsbag-rosbag.asd --- System definition for rosbag backend of rsbag.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag-rosbag-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsbag-rosbag-system)

#.(progn
    (load (merge-pathnames "cl-rsbag.asd" *load-truename*))
    (values))

(defsystem :cl-rsbag-rosbag
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "Rosbag file format backend for cl-rsbag."
  :depends-on  ((:version :cl-rsbag #.(cl-rsbag-system:version/string)))
  :components  ((:module     "rosbag"
                 :pathname   "src/backend/rosbag"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "generator")
                              (:file       "macros")
                              (:file       "spec")
                              (:file       "io")
                              (:file       "file")))))
