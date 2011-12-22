;;; cl-rsbag.asd ---
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:defpackage :cl-rsbag-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :cl-rsbag-system)


;;; Version stuff
;;

(defconstant +version-major+ 0
  "Major component of version number.")

(defconstant +version-minor+ 6
  "Minor component of version number.")

(defconstant +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definition
;;

(defsystem :cl-rsbag
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "Common Lisp implementation of rsbag."
  :defsystem-depends-on (:cl-protobuf)
  :depends-on  (:alexandria
		:split-sequence
		:metabang-bind
		:iterate
		:cl-dynamic-classes
		:local-time

		:bordeaux-threads

		:cl-protobuf

		(:version :cl-rsb #.(version/string)))
  :components  ((:module     "src/early"
		 :pathname   "src"
		 :components ((:file       "package")

			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "conditions"
			       :depends-on ("package"))))

		(:module     "backend"
		 :pathname   "src/backend"
		 :depends-on ("src/early")
		 :components ((:file       "package")
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "stream-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "direction-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "buffering-writer-mixin"
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

			      (:file       "util")

			      (:file       "channel"
			       :depends-on ("protocol" "util"))
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
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "multi-sequence-mixins"
			       :depends-on ("package"))
			      (:file       "elt-via-iterator-mixin"
			       :depends-on ("package"))

			      (:file       "serialized"
			       :depends-on ("package" "protocol"
					    "multi-sequence-mixins"
					    "elt-via-iterator-mixin"))))

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

			      (:file       "transform"
			       :pathname   "src/transform/rsb-event"
			       :depends-on ("protocol"))))

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

			      (:file       "channel-strategies"
			       :depends-on ("package" "protocol"))

			      (:file       "channel-connection"
			       :depends-on ("package" "protocol"))
			      (:file       "bag-connection"
			       :depends-on ("package" "protocol"))

			      (:file       "construction"
			       :depends-on ("package" "protocol"
					    "bag-connection"
					    "channel-connection"))))

		#+sbcl
		(:module     "rsb/replay"
		 :pathname   "src/rsb/replay"
		 :depends-on ("rsb")
		 :components ((:file       "package")

			      (:file       "util"
			       :depends-on ("package"))

			      (:file       "replay-restart-mixin"
			       :depends-on ("package"))
			      (:file       "error-policy-mixin"
			       :depends-on ("package"))
			      (:file       "bounds-mixin"
			       :depends-on ("package"))
			      (:file       "view-creation-mixin"
			       :depends-on ("package" "util"))
			      (:file       "sequential-mixin"
			       :depends-on ("package" "util"
					    "replay-restart-mixin"
					    "bounds-mixin"
					    "view-creation-mixin"))
			      (:file       "speed-adjustment-mixin"
			       :depends-on ("package"))
			      (:file       "timed-replay-mixin"
			       :depends-on ("package" "sequential-mixin"
					    "speed-adjustment-mixin"))
			      (:file       "external-driver-mixin"
			       :depends-on ("package" "sequential-mixin"))
			      (:file       "delay-correcting-mixin"
			       :depends-on ("package"))

			      (:file       "recorded-timing"
			       :depends-on ("package" "timed-replay-mixin"
					    "error-policy-mixin"
					    "delay-correcting-mixin"))
			      (:file       "fixed-rate"
			       :depends-on ("package" "timed-replay-mixin"
					    "error-policy-mixin"
					    "delay-correcting-mixin"))
			      (:file       "as-fast-as-possible"
			       :depends-on ("package" "sequential-mixin"
					    "error-policy-mixin"))
			      (:file       "interactive"
			       :depends-on ("package" "external-driver-mixin"
					    "error-policy-mixin"))
			      (:file       "remote-controlled"
			       :depends-on ("package" "external-driver-mixin"
					    "error-policy-mixin")))))

  :in-order-to ((test-op (test-op :cl-rsbag-test))))

(defsystem :cl-rsbag-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "Unit tests for the cl-rsbag system."
  :depends-on  ((:version :cl-rsbag #.(version/string))
		:lift)
  :components  ((:module     "test"
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "bag"
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
		 :components ((:file       "package")
			      (:file       "fixed-rate"
			       :depends-on ("package")))))

  :in-order-to ((test-op (load-op :cl-rsbag-test))))

(defmethod perform ((op     test-op)
		    (system (eql (find-system :cl-rsbag-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))


;;; TIDE log file format backend
;;

(defsystem :cl-rsbag-tidelog
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "TIDE log file format backend for cl-rsbag."
  :depends-on  ((:version :cl-rsbag #.(version/string)))
  :components  ((:module     "tidelog"
		 :pathname   "src/backend/tidelog"
		 :components ((:file       "package")
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "variables"
			       :depends-on ("package"))

			      (:file       "generator"
			       :depends-on ("package"))
			      (:file       "macros"
			       :depends-on ("package" "generator"))

			      (:file       "spec"
			       :depends-on ("package" "macros"))
			      (:file       "io"
			       :depends-on ("package" "conditions"
					    "spec"))

			      (:file       "index"
			       :depends-on ("package" "spec" "io"))
			      (:file       "file"
			       :depends-on ("package" "variables"
					    "spec" "io"))))))


;;; Elan file format backend
;;

(defsystem :cl-rsbag-elan
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "Elan file format backend for cl-rsbag."
  :depends-on  (:cxml-location

		(:version :cl-rsbag #.(version/string)))
  :components  ((:module     "elan"
		 :pathname   "src/backend/elan"
		 :components ((:file       "package")
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "xml"
			       :depends-on ("package" "types"))
			      (:file       "file"
			       :depends-on ("package" "types" "xml"))))))
