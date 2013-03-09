;;; protocol.lisp --- Protocol functions of the rsb.replay module.
;;
;; Copyright (C) 2013 Jan Moringen
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

;;; Bounds protocol

(defgeneric strategy-start-index (strategy)
  (:documentation
   "Return the start index of the region processed by STRATEGY."))

(defgeneric strategy-end-index (strategy)
  (:documentation
   "Return the end index of the region processed by STRATEGY."))

;;; Fixed-rate strategy protocol

(defgeneric strategy-delay (strategy)
  (:documentation
   "Return the delay in seconds between events replayed with
STRATEGY."))

(defgeneric (setf strategy-delay) (new-value strategy)
  (:documentation
   "Set the delay in seconds between events replayed with STRATEGY to
NEW-VALUE."))

(defgeneric strategy-rate (strategy)
  (:documentation
   "Return the rate in Hertz of events replayed with STRATEGY."))

(defgeneric (setf strategy-rate) (new-value strategy)
  (:documentation
   "Set the rate in Hertz of events replayed with STRATEGY to
NEW-VALUE."))
