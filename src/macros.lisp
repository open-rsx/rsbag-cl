;;;; macros.lisp --- Convenience macros provided by the rsbag system.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defun call-with-open-bag (bag thunk)
  "Call THUNK with BAG as the sole argument. Close BAG when THUNK
   returns or a control transfer occurs."
  (unwind-protect
       (funcall thunk bag)
    (close bag)))

(defmacro with-open-bag ((var bag) &body body)
  "Execute BODY with VAR bound to BAG. The bag is closed when BODY
   finishes or a control transfer occurs."
  (check-type var symbol "a symbol")

  `(flet ((with-open-bag-thunk (,var) ,@body))
     (declare (dynamic-extent #'with-open-bag-thunk))
     (call-with-open-bag ,bag #'with-open-bag-thunk)))

(defmacro with-bag ((var source &rest args &key &allow-other-keys)
                    &body body)
  "Execute BODY with VAR bound to a bag object for the data source
   SOURCE. ARGS are passed to `open-bag'."
  (check-type var symbol "a symbol")

  `(with-open-bag (,var (open-bag ,source ,@args)) ,@body))
