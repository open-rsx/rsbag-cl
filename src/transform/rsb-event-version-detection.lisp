;;;; rsb-event-version-detection.lisp --- Try multiple serialization versions.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.transform)

(defvar *serialization-versions* '()
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
  (log:info "~@<Forced to use auto-detection of event serialization ~
             format; version ~S with arguments ~{~S~^, ~}.~@:>"
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
                    should be tried when encoding or decoding
                    entries. Each candidate is of the form

                      (SPEC . ARGS)

                    where SPEC designates a serialization and ARGS is
                    a list of arguments that is passed to the
                    serialization during instantiation.")
   (implementation :initarg  :implementation
                   :accessor transform-%implementation
                   :initform nil
                   :documentation
                   "Stores a transform instance which implements the
                    serialization currently being tried or nil before
                    the first instance has been constructed. The
                    instance is replaced by the next candidate when it
                    produces an error.")
   (problems       :type     list
                   :accessor transform-%problems
                   :initform '()
                   :documentation
                   "Stores a list of problems encountered when trying
                    serialization versions. Elements are of the form

                      (ACTION IMPLEMENTATION CONDITION)

                    where ACTION is one of the keywords :INSTANTIATE
                    and :USE."))
  (:default-initargs
   :candidates (missing-required-initarg
                'rsb-event/version-detection :candidates))
  (:documentation
   "Instances of this transform class try to encode and decode events
    by dispatching the encoding or decoding task to a sequence of
    subsequent serialization implementations until one of those
    succeeds."))

(defmethod next-implementation! ((transform rsb-event/version-detection))
  (let+ (((&accessors (candidates     transform-candidates)
                      (implementation transform-%implementation)
                      (problems       transform-%problems))
          transform))
    (tagbody
     :start
       (when-let ((candidate (pop candidates)))
         (log:info "~@<Trying ~A~@:>" candidate)
         (handler-bind
             ((error (lambda (condition)
                       (appendf problems (list :instantiate candidate condition))
                       (log:info "~@<Failed to instantiate ~A with args ~{~A~^ ~}: ~A~@:>"
                                 (first candidate) (rest candidate) condition)
                       (go :start))))
           (return-from next-implementation!
             (setf implementation (apply #'make-transform candidate))))))))

(defmethod transform-implementation ((transform rsb-event/version-detection))
  (or (transform-%implementation transform)
      (next-implementation! transform)))

(macrolet
    ((define-try-method (name &rest args)
       `(defmethod ,name ((transform rsb-event/version-detection)
                          ,@args)
          (let+ (((&accessors (implementation transform-implementation)
                              (problems       transform-%problems))
                  transform))
            (tagbody
             :start
               (handler-bind
                   ((error (lambda (condition)
                             (appendf problems (list (list :use implementation condition)))
                             (log:info "~@<~A failed with ~A: ~A~@:>"
                                       ',name implementation condition)
                             (if (next-implementation! transform)
                                 (go :start)
                                 (error "~@<Could not detect ~
                                        serialization ~
                                        version:~{~2&~{When ~Aing ~
                                        ~S:~&~A~}~}~@:>"
                                        problems)))))
                 (return-from ,name
                   (,name implementation ,@(mapcar #'first args)))))))))

  (define-try-method transform-name)
  (define-try-method decode         (data t))
  (define-try-method encode         (data t)))
