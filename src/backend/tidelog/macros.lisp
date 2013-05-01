;;;; macros.lisp --- Macros for code generation.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.tidelog)

(defmacro define-element ((name) &body specs-and-options)
  (check-type name symbol "a symbol")

  (let+ (((&values specs (&plist-r/o (documentation :documentation)
                                     (toplevel?     :toplevel?)))
          (parse-specs-and-options specs-and-options)))
    `(progn
       ,(specs->class name specs :toplevel?     toplevel?
                                 :documentation documentation)
       ,(specs->size name specs)
       ,(specs->serializer name specs :toplevel? toplevel?)
       ,(specs->deserializer name specs))))

(defun parse-specs-and-options (specs-and-options)
  (let ((specs   (remove-if     #'keywordp specs-and-options
                                :key #'first))
        (options (remove-if-not #'keywordp specs-and-options
                                :key #'first)))
    (values specs (apply #'append options))))
