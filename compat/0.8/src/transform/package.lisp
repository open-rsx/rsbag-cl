;;; package.lisp --- Package definition for the transform module.
;;
;; Copyright (C) 2011-2018 Jan Moringen
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

(cl:defpackage :rsbag.transform
  (:use
   :cl
   :alexandria
   :let-plus
   :iterate
   :more-conditions

   :nibbles)

  (:import-from :rsbag
   :make-versioned-name
   :with-versioned-packages)

  ;; Variables
  (:export
   :+rsb-schema-name+)

  ;; Conditions
  (:export
   :transform-error
   :transform-error-transform

   :encoding-error
   :transform-error-domain-object

   :decoding-error
   :transform-error-encoded)

  ;; Transform protocol
  (:export
   :transform-name
   :transform-format

   :decode
   :encode)

  (:documentation
   "This package contains the transformation protocol and
infrastructure used in rsbag."))
