;;;; mock-backend.lisp --- Mock backend class for unit tests.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.test)

(defclass mock-backend ()
  ((channels :initarg  :channels
             :type     list
             :accessor backend-%channels
             :initform '()
             :documentation
             "List of mock-channel data of the form

                (TIMESTAMPS ENTRIES ID NAME META-DATA)

              where

              TIMESTAMPS is a list of `local-time:timestamp' objects,

              ENTRIES is a list of raw entries,

              ID is the integer id of the channel,

              NAME is a string naming the channel,

              and META-DATA is a plist containing the meta-data for
              the channel.")))

(service-provider:register-provider/class
 'rsbag.backend::backend :mock :class 'mock-backend)

(defmethod shared-initialize :after ((instance   mock-backend)
                                     (slot-names t)
                                     &key &allow-other-keys))

(defmethod close ((stream mock-backend) &key abort)
  (declare (ignore abort)))

(defmethod rsbag.backend:backend-location ((backend mock-backend))
  nil)

(defmethod rsbag.backend:get-channels ((backend mock-backend))
  (mapcar (curry #'nthcdr 2) (backend-%channels backend)))

(defmethod rsbag.backend:make-channel-id ((backend mock-backend)
                                          (channel t))
  (length (backend-%channels backend)))

(defmethod rsbag.backend:put-channel ((backend   mock-backend)
                                      (channel   integer)
                                      (name      string)
                                      (meta-data list))
  (appendf (backend-%channels backend)
           (list (list nil nil channel name meta-data))))

(defmethod rsbag.backend:get-num-entries ((backend mock-backend)
                                          (channel integer))
  (length (rsbag.backend:get-timestamps backend channel)))

(defmethod rsbag.backend:get-timestamps ((backend mock-backend)
                                         (channel integer))
  (first (nth channel (backend-%channels backend))))

(defmethod rsbag.backend:get-entry-at-index ((backend mock-backend)
                                             (channel integer)
                                             (index   integer))
  (let* ((channel (nth channel (backend-%channels backend)))
         (entry   (nth index (second channel))))
    (case entry
      (error (error "~@<Simulated entry retrieval error~@:>"))
      (t     entry))))

(defmethod rsbag.backend:put-entry ((backend   mock-backend)
                                    (channel   integer)
                                    (timestamp integer)
                                    (entry     t))
  (let ((channel (nth channel (backend-%channels backend))))
    (appendf (first channel) (list timestamp))
    (appendf (second channel) (list entry))))

;;; Convenience macros

(defmacro with-mock-backend ((backend-var) content &body body)
  "Execute BODY with BACKEND-VAR bound to a `mock-backend' instance
   filled with CONTENT."
  `(let ((,backend-var (make-instance 'mock-backend
                                      :channels ,content)))
     ,@body))

(defmacro with-mock-bag ((bag-var &rest initargs) content &body body)
  "Execute BODY with BAG-VAR bound to a `bag' instance backed by a
   `mock-backend' instance filled with CONTENT. INITARGS are passed to
   the constructed `bag' instance."
  (with-gensyms (backend-var)
    `(with-mock-backend (,backend-var) ,content
       (let ((,bag-var (make-instance 'bag
                                      :backend ,backend-var
                                      ,@initargs)))
         ,@body))))

(defun simple-channels (&key (errors '()))
  (let+ (((&flet maybe-error (value)
            (if (member value errors) 'error value)))
         (now (local-time:now))
         ((&flet now+ (amount)
            (rsbag.backend:timestamp->uint64
             (local-time:adjust-timestamp now
               (:offset :nsec amount))))))
    `(((,(now+ 0) ,(now+ 50000000))
      ,(mapcar #'maybe-error '(1 2))
      0 "/foo"
      ())
      ((,(now+ 50001000) ,(now+ 50002000) ,(now+ 100001000))
      ,(mapcar #'maybe-error '(3 4 5))
      1 "/bar"
      ()))))

(defun simple-bag (&key (errors '()))
  (with-mock-bag (bag :direction :input) (simple-channels :errors errors)
    bag))
