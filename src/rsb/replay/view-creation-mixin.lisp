;;; view-creation-mixin.lisp --- Mixin class for creating multi-channel views.
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

(cl:in-package :rsbag.rsb.replay)

(defclass view-creation-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that have to construct a view sequence for multiple channels. The
generic function `make-view' can be used to customize this
behavior. The method for `view-creation-mixin' creates a
serialized view of events across channels."))

(defmethod make-view ((connection replay-bag-connection)
		      (strategy   view-creation-mixin)
		      &key
		      (selector (rcurry #'inject-informer connection)))
  "Default behavior is serializing events across channels."
  (make-serialized-view
   (mappend #'connection-channels (connection-channels connection))
   :selector selector))
