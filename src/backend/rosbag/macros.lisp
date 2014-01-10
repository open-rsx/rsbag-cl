;;;; macros.lisp --- Macros for code generation.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.rosbag)

(defun parse-specs-and-options (specs-and-options)
  (let ((specs   (remove-if     #'keywordp specs-and-options
                                :key #'first))
        (options (remove-if-not #'keywordp specs-and-options
                                :key #'first)))
    (values specs (apply #'append options))))

(defmacro define-record ((name &key (opcode nil opcode-supplied?))
                         &body specs-and-options)
  (check-type name symbol "a symbol")
  (when opcode-supplied?
    (check-type opcode octet))

  (let+ (((&values specs (&plist-r/o (documentation :documentation)))
          (parse-specs-and-options specs-and-options))
         (all-specs specs))
    `(progn
       ,(specs->class name all-specs :documentation documentation)
       ,@(when opcode-supplied?
           `((setf (find-record-class ,opcode) (find-class ',name))))
       ,(specs->size name all-specs)
       ,(specs->serializer name all-specs)
       ,(specs->deserializer name all-specs))))
