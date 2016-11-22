;;;; rsbag-builder.asd --- Builder support for RSBag objects.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag-builder-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsbag-builder-system)

#.(progn
    (load (merge-pathnames "cl-rsbag.asd" *load-truename*))
    (values))

(defsystem :rsbag-builder
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsbag-system:version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Builder support for RSBag objects such as bags and channels."
  :depends-on  (:alexandria
                :let-plus

                (:version :architecture.builder-protocol "0.3")

                (:version :cl-rsbag                      #.(cl-rsbag-system:version/string)))
  :components  ((:module     "builder"
                 :pathname   "src"
                 :components ((:file       "builder"))))

  :in-order-to ((test-op (test-op :rsbag-builder/test))))

(defsystem :rsbag-builder/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsbag-system:version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsbag-builder system."
  :depends-on  ((:version :lift                               "1.7.1")

                (:version :architecture.builder-protocol/test "0.3")

                (:version :rsbag-builder                      #.(cl-rsbag-system:version/string))

                (:version :cl-rsbag/test                      #.(cl-rsbag-system:version/string)))
  :components  ((:module     "builder"
                 :pathname   "test"
                 :components ((:file       "builder")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsbag-builder/test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
           :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                            "lift-builder.config")))
