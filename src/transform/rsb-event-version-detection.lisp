;;; rsb-event-version-detection.lisp --- Try multiple serialization versions.
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

(cl:in-package :rsbag.transform)

(defvar *serialization-versions* (list +rsb-schema-name+)
  "List of symbols designating RSB event serialization
versions. Serializations are tried in the specified order until one
succeeds.")

(defgeneric next-implementation! (transform)
  (:documentation
   "Instantiate and install the next candidate serialization
implementation (if any) in TRANSFORM."))

(defmethod make-transform ((spec (eql :|rsb-event|))
			   &rest args)
  "Return event serialization version 0.4."
  (when args
    (warn "~@<Serialization version ~A does not take arguments (but ~
arguments ~A have been supplied).~:@>"
	  :rsb-event-0.4 args))
  (make-transform :rsb-event-0.4 :utf-8-string))

(defmethod make-transform ((spec (eql :rsb-event))
			   &rest args)
  "Return an instance of `rsb-event/version-detection' which passes
ARGS to candidate serialization implementations."
  (rsb:log1 :info "Forced to use auto-detection of event serialization ~
format; version ~S with arguments ~{~S~^, ~}."
	    spec args)
  (make-instance
   'rsb-event/version-detection
   :candidates (map 'list #'cons
		    *serialization-versions* (circular-list args))))

(defclass rsb-event/version-detection ()
  ((candidates     :initarg  :candidates
		   :accessor transform-candidates
		   :type     list
		   :documentation
		   "Stores a list of candidate transformations that
should be tried when encoding or decoding entries. Each candidate is
of the form

  (SPEC . ARGS)

where SPEC designates a serialization and ARGS is a list of arguments
that is passed to the serialization during instantiation.")
   (implementation :initarg  :implementation
		   :accessor %transform-implementation
		   :initform nil
		   :documentation
		   "Stores a transform instance which implements the
serialization currently being tried or nil before the first instance
has been constructed. The instance is replaced by the next candidate
when it produces an error."))
  (:default-initargs
   :candidates (required-argument :candidates))
  (:documentation
   "Instances of this transform class try to encode and decode events
by dispatching the encoding or decoding task to a sequence of
subsequent serialization implementations until one of those
succeeds."))

(defmethod next-implementation! ((transform rsb-event/version-detection))
  (bind (((:accessors (candidates      transform-candidates)
		      (%implementation %transform-implementation))
	  transform))
    (tagbody
     :start
       (when-let ((candidate (pop candidates)))
	 (rsb:log1 :info "Trying ~A" candidate)
	 (handler-bind
	     ((error (lambda (condition)
		       (rsb:log1 :info "Failed to instantiate ~A with args ~{~A~^ ~}: ~A"
				 (first candidate) (rest candidate) condition)
		       (go :start))))
	   (return-from next-implementation!
	     (setf %implementation (apply #'make-transform candidate))))))))

(defmethod transform-implementation ((transform rsb-event/version-detection))
  (or (%transform-implementation transform)
      (next-implementation! transform)))

(macrolet
    ((define-try-method (name &rest args)
       `(defmethod ,name ((transform rsb-event/version-detection)
			  ,@args)
	  (bind (((:accessors (implementation transform-implementation))
		  transform))
	    (tagbody
	     :start
	       (handler-bind
		   ((error (lambda (condition)
			     (declare (ignore condition))
			     (rsb:log1 :info "~A failed with ~A"
				       ',name implementation)
			     (when (next-implementation! transform)
			       (go :start)))))
		 (return-from ,name
		   (,name implementation ,@(mapcar #'first args)))))))))

  (define-try-method transform-name)
  (define-try-method decode         (data t))
  (define-try-method encode         (data t)))
