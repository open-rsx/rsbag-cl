;;;; types.lisp --- Types used in the cl-rsbag system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

;;; Bag opening and entry reading/writing options

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
