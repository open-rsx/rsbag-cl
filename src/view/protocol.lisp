;;; protocol.lisp --- Protocol for the view module.
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

(cl:in-package :rsbag.view)


;;; View construction functions
;;

(defgeneric make-serialized-view (sequences
				  &key
				  selector
				  compare)
  (:documentation
   "Make and return a sequence that consists of a serialization of the
elements of SEQUENCES. The serialization is performed by comparing
timestamps of elements arranging elements in the order of increasing
timestamps.

SELECTOR is a function that is applied to each element of SEQUENCES
before the view is constructed. When SEQUENCES is a sequence of
`channel's or a `bag', functions such as `channel-timestamps',
`channel-entries' and `channel-items' can be supplied as SELECTOR to
select the elements of the returned sequence.

COMPARE is a ordering predicate."))


;;; Extensible support functions
;;

(defgeneric %make-key-function (sequence)
  (:documentation
   "Return a function that takes four arguments, a sequence, an
iterator, a limit and a from-end-value, and returns a serialization
key (for example a `local-time:timestamp') for the corresponding
element. The returned function has to be applicable to sequences of
the type of SEQUENCE and associated iterators."))
