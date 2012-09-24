;;; macros.lisp --- Convenience macros provided by the cl-rsbag system.
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

(cl:in-package :rsbag)

(defmacro with-bag ((var source
		     &rest args
		     &key &allow-other-keys)
		    &body body)
  "Execute BODY with VAR bound to a bag object for the data source
SOURCE. ARGS are passed to `open-bag'."
  (check-type var symbol "a symbol")

  `(let ((,var (open-bag ,source ,@args)))
     (unwind-protect
	  (progn ,@body)
       (close ,var))))
