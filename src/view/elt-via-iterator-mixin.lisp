;;; elt-via-iterator-mixin.lisp --- Mixin class for non-random-access sequences
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

(cl:in-package :rsbag.view)

(defclass elt-via-iterator-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into sequence classes that
cannot provide `sequence:elt' directly but can provide
iterators. Subclasses inherit a method on `sequence:elt' that
positions an iterator on the requested index and retrieves the element
from it."))

(defmethod sequence:elt ((view  elt-via-iterator-mixin)
			 (index integer))
  ;; Create an iterator and advance it to INDEX.
  (let+ (((&values iterator &ign from-end)
	  (sequence:make-simple-sequence-iterator view)))
    (iter (repeat index) (sequence:iterator-step view iterator from-end))
    (sequence:iterator-element view iterator)))
