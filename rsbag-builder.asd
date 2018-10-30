;;;; rsbag-builder.asd --- Builder support for RSBag objects.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag-builder-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsbag-builder-system)

#.(progn
    (load (merge-pathnames "rsbag.asd" *load-truename*))
    (values))

(asdf:defsystem "rsbag-builder"
  :description "Builder support for RSBag objects such as bags and channels."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(rsbag-system:version/string)
  :depends-on  ("alexandria"
                "let-plus"

                (:version "architecture.builder-protocol" "0.3")

                (:version "rsbag"                         #.(rsbag-system:version/string)))

  :components  ((:module     "builder"
                 :pathname   "src"
                 :components ((:file       "builder"))))

  :in-order-to ((test-op (test-op "rsbag-builder/test"))))

(asdf:defsystem "rsbag-builder/test"
  :description "Unit tests for the rsbag-builder system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(rsbag-system:version/string)
  :depends-on  ((:version "lift"                               "1.7.1")

                (:version "architecture.builder-protocol/test" "0.3")

                (:version "rsbag-builder"                      #.(rsbag-system:version/string))

                (:version "rsbag/test"                         #.(rsbag-system:version/string)))

  :components  ((:module     "builder"
                 :pathname   "test"
                 :components ((:file       "builder"))))

  :perform     (test-op (operation component)
                 (funcall (find-symbol "RUN-TESTS" :lift)
                          :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                                           "lift-builder.config"))))
