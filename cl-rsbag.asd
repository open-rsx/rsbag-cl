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
   :asdf))

(cl:in-package :cl-rsbag-system)


;;; Version stuff
;;

(defconstant +version-major+ 0
  "Major component of version number.")

(defconstant +version-minor+ 1
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
  :depends-on  (:alexandria
		:metabang-bind
		:iterate
		:local-time
		:cl-protobuf ;; for binio
		)
  :components  ((:module     "backend"
		 :pathname   "src/backend"
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "stream-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "buffering-writer-mixin"
			       :depends-on ("package" "protocol"))))

		(:module     "src"
		 :depends-on ("backend")
		 :components ((:file "package")

			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "channel"
			       :depends-on ("package" "protocol"))
			      (:file       "bag"
			       :depends-on ("package" "protocol" "channel"))


			      (:file       "macros"
			       :depends-on ("package" "protocol")))))

  :in-order-to ((test-op (test-op :cl-rsbag-test))))

(defsystem :cl-rsbag-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "Unit tests for the cl-rsbag system."
  :depends-on  (:cl-rsbag
		:lift)
  :components  ((:module     "test"
		 :components ((:file       "package")
			      (:file       "bag"
			       :depends-on ("package")))))

  :in-order-to ((test-op (load-op :cl-rsbag-test))))

(defmethod perform ((op     test-op)
		    (system (eql (find-system :cl-rsbag-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))


;;; TIDE log backend
;;

(defsystem :cl-rsbag-tidelog
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "TIDE log file format backend for cl-rsbag."
  :depends-on  (:cl-rsbag)
  :components  ((:module     "tidelog"
		 :pathname   "src/backend/tidelog"
		 :components ((:file       "package")
			      (:file       "variables"
			       :depends-on ("package"))

			      (:file       "generator"
			       :depends-on ("package"))
			      (:file       "macros"
			       :depends-on ("package" "generator"))

			      (:file       "spec"
			       :depends-on ("package" "macros"))
			      (:file       "io"
			       :depends-on ("package" "spec"))


			      (:file       "index"
			       :depends-on ("package" "spec" "io"))
			      (:file       "file"
			       :depends-on ("package" "variables"
					    "spec" "io"))))))
