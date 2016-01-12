;;;; types.lisp --- Types used in the cl-rsbag Elan backend.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.elan)

(deftype timestamp/milliseconds ()
  "Time since reference timestamp in milliseconds."
  '(unsigned-byte 64))

(deftype timestamp/nanoseconds ()
  "Time since reference timestamp in nanoseconds."
  '(unsigned-byte 64))

(deftype time-slot/cons ()
  "A list of the form

     (ID TIMESTAMP)

   to be interpreted as an Elan time slot consisting of a string id
   and an associated `timestamp/nanoseconds'."
  '(cons string timestamp/nanoseconds))

(deftype annotation/list ()
  "A list of the form

     (ID START-TIME-SLOT END-TIME-SLOT DATUM)

   to be interpreted as an annotation consisting of two time slot
   string ids and an associated string datum."
  '(cons string (cons string (cons string (cons string null)))))

(deftype linguistic-type/list ()
  "A list of the form

     (ID GRAPHIC-REFERENCES TIME-ALIGNABLE)

   to be interpreted as a linguistic type."
  '(cons string (cons boolean (cons boolean null))))

(deftype tier/list ()
  "A list of the form

     (NAME LINGUISTIC-TYPE-REF ANNOTATIONS)

   to be interpreted as a named tier containing the annotation items
   ANNOTATIONS."
  '(cons string (cons string (cons list null))))

(deftype version/cons ()
  "A version specification of the form

     (MAJOR . MINOR)

   ."
  '(cons non-negative-integer non-negative-integer))

(deftype file/list ()
  "A list of the form

     (AUTHOR DATE MEDIA-URLS TIME-SLOTS TIERS)

   where MEDIA-URLS is a list of strings, TIME-SLOTS is a list of
   elements of type TIME-SLOT/CONS and TIERS is a list of elements of
   type TIER/LIST."
  '(cons string                                   ; author
         (cons local-time:timestamp               ; date in ISO timestamp format
               (cons list                         ; media URLs
                     (cons list                   ; time slots
                           (cons list))))))       ; tiers
