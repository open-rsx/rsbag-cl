;;; package.lisp --- Package definition for the backend.tidelog module.
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

(cl:defpackage :rsbag.backend.tidelog
  (:use
   :cl
   :alexandria
   :split-sequence
   :let-plus
   :iterate
   :more-conditions

   :rsbag
   :rsbag.backend)

  ;; Conditions
  (:export
   :tidelog-condition

   :tidelog-file-error

   :invalid-tidelog-structure)

  (:documentation
   "This package contains a cl-rsbag backend for the TIDE log file
format as specified at https://retf.info/svn/drafts/rd-0001.txt.

The most important interface classes are:
+ `file' which represents an entire TIDE log file (and handles
  access to its channels)
+ `index' which represents the index for one channel of a TIDE log
  file

In addition, there is lower-level input/output machinery based on code
generated according to the TIDE log specification. This level operates
on TIDE log blocks.

The interface consists of the generic functions
+ `scan' which finds blocks in a TIDE log file without reading their
  entire contents
+ `pack' which stores blocks in TIDE log files
+ `unpack' which reads blocks from TIDE log files

The code generation is implemented by
+ spec->* family of functions
+ `define-element' macro."))
