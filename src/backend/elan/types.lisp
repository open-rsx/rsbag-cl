;;; types.lisp --- Types used in the cl-rsbag Elan backend.
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

(cl:in-package :rsbag.backend.elan)

(deftype timestamp/milliseconds ()
  "Time since reference timestamp in milliseconds."
  '(unsigned-byte 64))

(deftype time-slot/cons ()
  "A list of the form

  (ID TIMESTAMP)

to be interpreted as an Elan time slot consisting of a string id and
an associated `timestamp/milliseconds'."
  '(cons string timestamp/milliseconds))

(deftype annotation/list ()
  "A list of the form

  (START-TIME-SLOT END-TIME-SLOT DATUM)

to be interpreted as an annotation consisting of two time slot string
ids and an associated string datum."
  '(cons string (cons string (cons string null))))

(deftype tier/list ()
  "A list of the form

  (NAME ANNOTATIONS)

to be interpreted as a named tier containing the annotation items
ANNOTATIONS."
  '(cons string (cons list null)))

(deftype file/list ()
  "A list of the form

  (DATE MEDIA-URLS TIME-SLOTS TIERS)

where MEDIA-URLS is a list of strings, TIME-SLOTS is a list of
elements of type TIME-SLOT/CONS and TIERS is a list of elements of
type TIER/LIST."
  '(cons local-time:timestamp        ;; date in ISO timestamp format
	 (cons list	             ;; media URLs
	       (cons list	     ;; time slots
		     (cons list))))) ;; tiers
