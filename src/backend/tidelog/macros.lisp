;;; macros.lisp --- Macros for code generation.
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

(cl:in-package :rsbag.backend.tidelog)

(defmacro define-element ((name) &body specs-and-options)
  (check-type name symbol "a symbol")

  (let+ (((&values specs (&plist-r/o (documentation :documentation)
				     (toplevel?     :toplevel?)))
	  (parse-specs-and-options specs-and-options)))
    `(progn
       ,(specs->class name specs :toplevel?     toplevel?
				 :documentation documentation)
       ,(specs->size name specs)
       ,(specs->serializer name specs :toplevel? toplevel?)
       ,(specs->deserializer name specs))))

(defun parse-specs-and-options (specs-and-options)
  (let ((specs   (remove-if     #'keywordp specs-and-options
				:key #'first))
	(options (remove-if-not #'keywordp specs-and-options
				:key #'first)))
    (values specs (apply #'append options))))
