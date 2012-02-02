;;; types.lisp --- Types used in the cl-rsbag Elan backend.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

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
