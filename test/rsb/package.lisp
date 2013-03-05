;;; package.lisp --- Package definition for unit tests of the rsb module.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:defpackage :rsbag.rsb.test
  (:use
   :cl
   :alexandria
   :let-plus
   :iterate
   :lift

   :nibbles

   :rsbag
   :rsbag.rsb
   :rsbag.rsb.replay

   :rsbag.test)

  (:documentation
   "This package contains unit tests for the rsb module."))

(cl:in-package :rsbag.rsb.test)

(deftestsuite rsb-root (root)
  ()
  (:documentation
   "Root unit test suite for the rsb module."))

(defmacro collecting-events ((name) &body body)
  "Execute BODY with a collector function named NAME in scope."
  (with-gensyms (collected)
    `(let+ ((,collected '())
	    ((&flet ,name (&optional datum)
	       (if datum
		   (push datum ,collected)
		   (reverse ,collected)))))
       ,@body)))

(defun select-channel (name)
  (compose (curry #'string= name) #'channel-name))

(defmacro define-replay-strategy-smoke-test
    ((class
      &key
      (suite-name (symbolicate class '#:-root)))
     &body
     cases)
  "Define a smoke test case for class CLASS in test suite `SUITE-NAME'
with CASES. Each element of CASES has to be of the form

  (INITARGS EXPECTED)

where EXPECTED is the list of entries the strategy should produce when
applied to `simple-bag'."
  `(addtest (,suite-name
	     :documentation
	     ,(format nil "Smoke test for the `~(~A~)' replay strategy class."
		      class))
     replay/smoke

     (ensure-cases (initargs expected)
	 (map-product
	  (lambda+ (initargs (initargs1 expected))
	    (list (append initargs initargs1) expected))
	  (list ,@cases)
	  `((()                                   (1 2 3 4 5))
	    ((:end-index   5)                     (1 2 3 4 5))
	    ((:start-index 1)                     (2 3 4 5))
	    ((:end-index   3)                     (1 2 3))
	    ((:channels ,(select-channel "/foo")) (1 2))))

       (collecting-events (collect)
	 (with-open-connection
	     (connection (apply #'bag->events (simple-bag) #'collect
				:replay-strategy ',class
				initargs))
	   (replay connection (connection-strategy connection)))
	 (ensure-same (collect) expected :test #'equal)))))
