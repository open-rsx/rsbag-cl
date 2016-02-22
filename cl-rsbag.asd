;;;; cl-rsbag.asd ---
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cl-rsbag-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string

   #:serialization-version/list
   #:serialization-version/string))

(cl:in-package #:cl-rsbag-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 13
  "Minor component of version number.")

(let* ((version-file (merge-pathnames "version.sexp" *load-truename*))
       stream)
  (when (probe-file version-file)
    (setf stream (open version-file)))

  (defparameter +version-revision+ (if stream (read stream) 0)
    "Revision component of version number.")

  (defparameter +version-commit+ (when stream (read stream))
    "Commit component of version number.")

  (when stream (close stream)))

(defun version/list (&key
                     (revision? t)
                     commit?)
  "Return a version of the form (MAJOR MINOR [REVISION [COMMIT]])
   where REVISION and COMMIT are optional.

   REVISION? controls whether REVISION should be included. Default
   behavior is to include REVISION.

   COMMIT? controls whether COMMIT should be included. Default
   behavior is to not include COMMIT."
  (append (list +version-major+ +version-minor+)
          (when revision? (list +version-revision+))
          (when (and commit? +version-commit+)
            (list +version-commit+))))

(defun version/string (&rest args
                       &key
                       revision?
                       commit?)
  "Return a version string of the form
   \"MAJOR.MINOR[.REVISION[-.COMMIT]]\" where REVISION and COMMIT are
   optional.

   See `version/list' for details on keyword parameters."
  (declare (ignore revision? commit?))
  (format nil "~{~A.~A~^.~A~^-~A~}" (apply #'version/list args)))

;;; Native serialization version
;;;
;;; We separate the serialization version from the system version to
;;; avoid unnecessary version bumps and the resulting bloat of
;;; multiple almost-compatible serialization versions.

(defparameter +serialization-version-major+ 0
  "Major component of the native serialization version number.")

(defparameter +serialization-version-minor+ 9
  "Minor component of the native serialization version number.")

(defun serialization-version/list ()
  "Return a version of the form (MAJOR MINOR)."
  (list +serialization-version-major+ +serialization-version-minor+))

(defun serialization-version/string ()
  "Return a version string of the form \"MAJOR.MINOR\".

   See `serialization-version/list' for details."
  (format nil "~{~A.~A~}" (serialization-version/list)))

;;; System definition

(defsystem :cl-rsbag
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Common Lisp implementation of rsbag."
  :defsystem-depends-on (:cl-protobuf)
  :depends-on  (:alexandria
                :split-sequence
                (:version :let-plus                      "0.2")
                :iterate
                (:version :architecture.service-provider "0.1")
                :more-conditions
                (:version :log4cl                        "1.1.1")
                (:version :utilities.print-items         "0.1")

                :bordeaux-threads
                :lparallel
                :local-time
                :nibbles

                (:version :cl-rsb                        #.(version/string :revision? nil))
                (:version :rsb-introspection             #.(version/string :revision? nil)))
  :components  ((:module     "src/early"
                 :pathname   "src"
                 :components ((:file       "package")

                              (:file       "types"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "util"
                               :depends-on ("package"))
                              (:file       "versioned-packages"
                               :depends-on ("package"))
                              (:file       "threadpool"
                               :depends-on ("package"))
                              (:file       "reloading"
                               :depends-on ("package" "threadpool"))))

                (:module     "backend"
                 :pathname   "src/backend"
                 :depends-on ("src/early")
                 :components ((:file       "package")
                              (:file       "util"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package"))

                              (:file       "backend-mixins"
                               :depends-on ("package" "protocol"))

                              (:file       "flush-strategies"
                               :depends-on ("package" "protocol"))))

                (:module     "transform"
                 :pathname   "src/transform"
                 :depends-on ("src/early")
                 :components ((:file       "package")

                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package" "conditions"))))

                (:module     "src"
                 :depends-on ("src/early" "backend" "transform")
                 :components ((:file       "protocol")

                              (:file       "channel"
                               :depends-on ("protocol"))
                              (:file       "bag"
                               :depends-on ("protocol" "channel"))

                              (:file       "synchronized-channel"
                               :depends-on ("channel"))
                              (:file       "synchronized-bag"
                               :depends-on ("bag" "synchronized-channel"))

                              (:file       "macros"
                               :depends-on ("protocol"))))

                #+sbcl
                (:module     "view"
                 :pathname   "src/view"
                 :depends-on ("src")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "mixins")
                              (:file       "serialized")))

                (:module     "rsb-serialization"
                 :pathname   "."
                 :depends-on ("transform")
                 :components ((:protocol-buffer-descriptor-directory "protocol"
                               :pathname   "data"
                               :components ((:file       "EventId"
                                             :pathname   "rsb/protocol/EventId")
                                            (:file       "EventMetaData"
                                             :pathname   "rsb/protocol/EventMetaData"
                                             :depends-on ("EventId"))
                                            (:file       "Notification"
                                             :pathname   "rsb/protocol/Notification"
                                             :depends-on ("EventId" "EventMetaData"))))

                              (:file       "rsb-event"
                               :pathname   "src/transform/rsb-event"
                               :depends-on ("protocol"))
                              (:file       "rsb-event-payload-conversion"
                               :pathname   "src/transform/rsb-event-payload-conversion"
                               :depends-on ("protocol" "rsb-event"))
                              (:file       "rsb-event-version-detection"
                               :pathname   "src/transform/rsb-event-version-detection"
                               :depends-on ("rsb-event"))
                              (:file       "rsb-event-multi-version"
                               :pathname   "src/transform/rsb-event-multi-version"
                               :depends-on ("rsb-event-version-detection"))))

                #+sbcl
                (:module     "rsb"
                 :pathname   "src/rsb"
                 :depends-on ("src" "transform" "view"
                              "rsb-serialization")
                 :components ((:file       "package")
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package"))
                              (:file       "macros"
                               :depends-on ("package" "protocol"))

                              (:file       "channel-connection"
                               :depends-on ("package" "protocol"))
                              (:file       "bag-connection"
                               :depends-on ("package" "protocol"))

                              (:file       "channel-strategies"
                               :depends-on ("package" "protocol"
                                            "channel-connection"))

                              (:file       "construction"
                               :depends-on ("package" "protocol"
                                            "bag-connection"
                                            "channel-connection"))))

                #+sbcl
                (:module     "rsb/replay"
                 :pathname   "src/rsb/replay"
                 :depends-on ("rsb")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "util")
                              (:file       "protocol")

                              (:file       "strategy-mixins")

                              (:file       "recorded-timing")
                              (:file       "fixed-rate")
                              (:file       "as-fast-as-possible")
                              (:file       "interactive")
                              (:file       "remote-controlled"))))

  :in-order-to ((test-op (test-op :cl-rsbag-test))))

(defsystem :cl-rsbag-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the cl-rsbag system."
  :depends-on  (:flexi-streams
                (:version :lift                    "1.7.1")

                (:version :cl-rsbag                #.(version/string))

                (:version :rsb-transport-inprocess #.(version/string :revision? nil)))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "mock-backend")
                              (:file       "protocol")
                              (:file       "channel")
                              (:file       "bag")))

                (:module     "backend"
                 :pathname   "test/backend"
                 :depends-on ("test")
                 :components ((:file       "package")
                              (:file       "flush-strategies"
                               :depends-on ("package"))
                              (:file       "mixins"
                               :depends-on ("package"))))

                #+sbcl
                (:module     "view"
                 :pathname   "test/view"
                 :depends-on ("test")
                 :components ((:file       "package")
                              (:file       "serialized"
                               :depends-on ("package"))))

                #+sbcl
                (:module     "rsb"
                 :pathname   "test/rsb"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "strategy-mixins")

                              (:file       "recorded-timing")
                              (:file       "fixed-rate")
                              (:file       "as-fast-as-possible")
                              (:file       "interactive")))))

(defmethod perform ((op     test-op)
                    (system (eql (find-system :cl-rsbag-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
