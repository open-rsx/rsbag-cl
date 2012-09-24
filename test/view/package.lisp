;;; package.lisp --- Package definition for unit tests of the view module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:defpackage :rsbag.view.test
  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :lift

   :rsbag
   :rsbag.view

   :rsbag.test)

  (:export
   :view-root)

  (:documentation
   "This package contains unit tests for the view module."))

(cl:in-package :rsbag.view.test)

(deftestsuite view-root (root)
  ()
  (:documentation
   "Root unit test suite for the view module."))
