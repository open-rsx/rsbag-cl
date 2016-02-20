;;;; flush-strategies.lisp --- Flush strategy classes provided by backend module.
;;;;
;;;; Copyright (C) 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend)

;;; `property-limit' strategy class

(defclass property-limit ()
  ((property :initarg  :property
             :type     keyword
             :accessor flush-strategy-property
             :documentation
             "Stores the name of the buffer property based on whose
              value the flushing decision should be made.")
   (limit    :initarg  :limit
             :type     real
             :accessor flush-strategy-limit
             :documentation
             "Stores the "))
  (:default-initargs
   :property (missing-required-initarg 'property-limit :property)
   :limit    (missing-required-initarg 'property-limit :limit))
  (:documentation
   "This strategy causes a buffer to be flushed every time a specified
    property violates a given limit."))

(service-provider:register-provider/class
 'flush-strategy :property-limit :class 'property-limit)

(defmethod shared-initialize :before ((instance   property-limit)
                                      (slot-names t)
                                      &key
                                      property)
  ;; Search methods on `buffer-property' for one that is specialized
  ;; on PROPERTY.
  (let+ (((&flet property? (specializer)
            (and (typep specializer 'eql-specializer)
                 (eq (eql-specializer-object specializer) property)))))
    (unless (some (compose #'property? #'third #'method-specializers)
                  (generic-function-methods #'buffer-property))
      (error "~@<Specified property ~A is invalid.~@:>"
             property))))

(defmethod flush? ((strategy property-limit)
                   (backend  t)
                   (buffer   t))
  (let+ (((&structure-r/o flush-strategy- property limit) strategy))
    (> (buffer-property backend buffer property) limit)))

(defmethod print-object ((object property-limit) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S > ~:D"
            (flush-strategy-property object)
            (flush-strategy-limit    object))))

;;; `composite-flush-strategy-mixin' mixin class

(defclass composite-flush-strategy-mixin ()
  ((children :initarg  :children
             :type     list
             :accessor flush-strategy-children
             :initform nil
             :documentation
             "A list of child strategies which are consulted to
              produce a decision."))
  (:documentation
   "This class is intended to be mixed into flush strategy classes
    which produce their decisions by consulting subordinate
    strategies."))

(defmethod print-object ((object composite-flush-strategy-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A (~D)"
            (class-name (class-of object))
            (length (flush-strategy-children object)))))

(macrolet
    ((define-simple-composite-strategy (name
                                        &key
                                        (spec       (make-keyword name))
                                        (class-name (format-symbol "FLUSH-IF-~A" name))
                                        reducer)
       `(progn
          (defclass ,class-name (composite-flush-strategy-mixin)
            ()
            (:documentation
             ,(format nil "This strategy flushes buffers when ~A of ~
                           its child strategies indicate that buffers ~
                           should be flushed."
                      reducer)))

          (service-provider:register-provider/class
           'flush-strategy ,spec :class ',class-name)

          (defmethod service-provider:make-provider
              ((service  t)
               (provider (eql (service-provider:find-provider 'flush-strategy ,spec)))
               &rest args)
            (make-instance
             (service-provider:provider-class provider)
             :children (mapcar (curry #'apply #'make-flush-strategy)
                               args)))

          (defmethod flush? ((strategy ,class-name)
                             (backend  t)
                             (buffer   t))
            (,reducer (rcurry #'flush? backend buffer)
                      (flush-strategy-children strategy))))))

  (define-simple-composite-strategy or
    :class-name flush-if-some
    :reducer    some)
  (define-simple-composite-strategy and
    :class-name flush-if-every
    :reducer    every)
  (define-simple-composite-strategy not
    :class-name flush-if-notany
    :reducer    notany))
