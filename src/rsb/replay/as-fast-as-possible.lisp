;;; as-fast-as-possible.lisp ---
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


;;; `as-fast-as-possible' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :as-fast-as-possible)))
  (find-class 'as-fast-as-possible))

(defclass as-fast-as-possible (error-policy-mixin
			       sequential-mixin
			       timestamp-adjustment-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded, but
as fast as possible. Consequently, recorded timestamps are only used
to establish the playback order of events, but not for any kind of
replay timing."))
