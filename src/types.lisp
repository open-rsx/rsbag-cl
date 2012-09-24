;;; types.lisp --- Types used in the cl-rsbag system.
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


;;; Bag opening and entry reading/writing options
;;

(deftype direction ()
  "Values of this type are used to indicate whether a bag should be
opened for reading, writing or both."
  '(member :input :output :io))

(deftype if-does-not-exist-policy ()
  "Possible actions to execute if a requested object does not exist."
  '(member nil :error))

(deftype if-exists-policy ()
  "Possible actions to execute if an object should be stored in
location that is already occupied."
  '(member :error :supersede))


;;; Transformation specifications
;;

(deftype transform-spec/default ()
  "This transform specification causes the default transformation to
be applied."
  'null)

(deftype transform-spec/augment ()
  "This transform specification causes supplied arguments to be
appended when the default transformation is constructed."
  '(cons (eql &from-source) list))

(deftype transform-spec/full ()
  "This transform specification cases the a specific transform to be
constructed with supplied arguments without automatic derivation."
  '(cons (and symbol (not (eql &from-source))) list))

(deftype transform-spec ()
  "All forms of transform specifications."
  '(or transform-spec/default
       transform-spec/augment
       transform-spec/full))
