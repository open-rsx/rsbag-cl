;;; speed-adjustment-mixin.lisp --- Mixin that scales scheduled playback times.
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

(defclass speed-adjustment-mixin ()
  ((speed :initarg  :speed
	  :type     positive-real
	  :accessor strategy-speed
	  :initform 1
	  :documentation
	  "Stores the speed factor that should be applied to the
results of scheduling events."))
  (:documentation
   "This mixin class adds to timed replay strategy classes the ability
to speed up or slow down replay speed by a constant factor."))

(defmethod schedule-event :around ((strategy speed-adjustment-mixin)
				   (event    t)
				   (previous local-time:timestamp)
				   (next     local-time:timestamp))
  (/ (call-next-method) (strategy-speed strategy)))
