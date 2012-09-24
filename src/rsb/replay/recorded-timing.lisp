;;; recorded-timing.lisp ---
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


;;; `recorded-timing' replay strategy class
;;

(defmethod find-replay-strategy-class ((spec (eql :recorded-timing)))
  (find-class 'recorded-timing))

(defclass recorded-timing (error-policy-mixin
			   timed-replay-mixin
			   delay-correcting-mixin
			   speed-adjustment-mixin
			   timestamp-adjustment-mixin)
  ()
  (:documentation
   "This strategy replays events in the order they were recorded and,
as much as possible, with identical local temporal relations. A
faithful replay with respect to global temporal relations (e.g. time
between first and last event) is not attempted explicitly."))

(defmethod schedule-event ((strategy recorded-timing)
			   (event    t)
			   (previous local-time:timestamp)
			   (next     local-time:timestamp))
  (local-time:timestamp-difference next previous))
