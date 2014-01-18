;;;; bag.lisp --- The bag class represent data channels stored in a file.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defclass bag ()
  ((direction :initarg  :direction
              :type     direction
              :reader   bag-direction
              :documentation
              "Stores the direction of the bag.")
   (backend   :initarg  :backend
              :reader   bag-%backend
              :documentation
              "Stores an object which is responsible for accessing the
               stream associated to this bag.")
   (transform :initarg  :transform
              :type     transform-spec
              :reader   bag-transform
              :accessor bag-%transform
              :initform '(&from-source)
              :documentation
              "Stores a specification for transformations that should
               be associated with channels of the bag. See type
               `transform-spec'.")
   (channels  :type     hash-table
              :reader   bag-%channels
              :initform (make-hash-table :test #'equal)
              :documentation
              "Stores a mapping of channel names to `channel'
               instances."))
  (:default-initargs
   :direction (missing-required-initarg 'bag :direction)
   :backend   (missing-required-initarg 'bag :backend))
  (:documentation
   "Instances of this class represent a log file. A log file consists
    of named channels which can be retrieved using the `bag-channels'
    and `bag-channel' methods and modified using the `(setf
    bag-channel)' method."))

(defmethod shared-initialize :after ((instance   bag)
                                     (slot-names t)
                                     &key)
  (let+ (((&accessors-r/o (backend   bag-%backend)
                          (transform bag-%transform)
                          (channels  bag-%channels)) instance)
         ((&flet make-transform (name meta-data id)
            (%make-channel-transform instance name meta-data
                                     :id   id
                                     :spec transform))))
    (iter (for (id name meta-data) in (rsbag.backend:get-channels backend))
          (setf (gethash name channels)
                (%make-channel instance name meta-data
                               (make-transform name meta-data id)
                               :id id)))))

(defmethod close ((bag bag)
                  &key &allow-other-keys)
  (close (bag-%backend bag)))

(defmethod bag-location ((bag bag))
  (rsbag.backend:backend-location (bag-%backend bag)))

(defmethod bag-channels ((bag bag))
  (hash-table-values (bag-%channels bag)))

(defmethod bag-channel ((bag bag) (name string)
                        &key
                        (if-does-not-exist :error))
  (or (gethash name (bag-%channels bag))
      (ecase if-does-not-exist
        (:error (error 'no-such-channel
                       :bag  bag
                       :name name))
        ((nil)  nil))))

(defmethod (setf bag-channel) :before ((new-value t)
                                       (bag       t)
                                       (name      t)
                                       &key &allow-other-keys)
  (when (eq (bag-direction bag) :input)
    (error 'read-only-bag
           :bag bag)))

(defmethod (setf bag-channel) ((new-value list)
                               (bag       bag)
                               (name      string)
                               &key
                               (if-exists :error)
                               (transform (%make-channel-transform
                                           bag name new-value
                                           :spec (bag-transform bag))))
  ;; If a channel named NAME already exists, apply IF-EXISTS policy.
  (when-let ((channel (gethash name (bag-%channels bag))))
    (ecase if-exists
      (:error     (error 'channel-exists
                         :bag     bag
                         :channel channel))
      (:supersede (error "Superseding not implemented")))) ; TODO(jmoringe): implement

  ;; If NEW-VALUE does not have a type, but TRANSFORM is non-nil,
  ;; augment the meta-data with TRANSFORM's type. Make a channel
  ;; instance and store it.
  (let+ (((&accessors-r/o (channels bag-%channels)
                          (backend  bag-%backend)) bag)
         (meta-data (if (and transform (not (getf new-value :type)))
                        (append (list :type (rsbag.transform:transform-name transform))
                                new-value)
                        new-value))
         (channel   (%make-channel bag name meta-data transform)))
    (rsbag.backend:put-channel
     backend (channel-%id channel) name meta-data)
    (setf (gethash name channels) channel)))

(defmethod print-object ((object bag) stream)
  (let+ (((&accessors-r/o (location  bag-location)
                          (direction bag-direction)
                          (channels  bag-%channels)) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~/rsbag:print-location/ ~/rsbag:print-direction/ (~D)"
              location direction (hash-table-count channels)))))

;;; Time range protocol

(macrolet ((define-bound-method (name comparator)
             `(defmethod ,name ((bag bag))
                (flet ((safe-compare (left right)
                         (cond
                           ((not left)               right)
                           ((not right)              left)
                           ((,comparator left right) left)
                           (t                        right))))
                  (reduce #'safe-compare (bag-channels bag)
                          :key           (function ,name)
                          :initial-value nil)))))
  (define-bound-method start-timestamp local-time:timestamp<)
  (define-bound-method end-timestamp   local-time:timestamp>))

;;;

(defmethod bag-channel-class ((bag bag))
  (find-class 'channel))

(defmethod %make-channel ((bag       bag)
                          (name      string)
                          (meta-data list)
                          (transform t)
                          &rest args
                          &key
                          id)
  (let+ (((&accessors-r/o (backend       bag-%backend)
                          (channel-class bag-channel-class)) bag))
    (with-condition-translation
        (((error channel-open-error)
          :bag     bag
          :channel name))
      (apply #'make-instance channel-class
             :bag       bag
             :name      name
             :transform transform
             :id        (or id (rsbag.backend:make-channel-id
                                backend name))
             :meta-data meta-data
             :backend   backend
             (remove-from-plist args :id)))))

(defmethod %make-channel-transform ((bag       bag)
                                    (name      string)
                                    (meta-data list)
                                    &key
                                    id
                                    spec)
  "Use SPEC and, optionally the :type entry of META-DATA to determine
   the appropriate transform for the channel designated by NAME.

   SPEC can be of the following types:

     `transform-spec/default'

       Use the :type entry of META-DATA to determine the appropriate
       transform.

     `transform-spec/augment'

       Append to the :type entry of META-DATA the remainder of
       SPEC. This will instantiate the transform class specified by
       the :type entry, but append initargs given in SPEC.

     `transform-spec/full'

       Use the contents of SPEC as class name and initargs to
       instantiate the transform class. Ignore :type entry of
       META-DATA."
  (declare (ignore id))

  (with-condition-translation
      (((error channel-open-error)
        :bag     bag
        :channel name))
    (restart-case
        (let+ (((&plist-r/o (type :type)) meta-data)
               ((&flet parse-type ()
                  (typecase type
                    (null (list nil))
                    (list type)
                    (t    (ensure-list type)))))
               ((class-name &rest args)
                (etypecase spec
                  ;; No spec - derive everything from TYPE.
                  (transform-spec/default
                   (parse-type))

                  ;; Spec with &FROM-SOURCE - append rest of SPEC to
                  ;; information derived from TYPE.
                  (transform-spec/augment
                   (append (parse-type) (rest spec)))

                  ;; Spec without &FROM-SOURCE - ignore TYPE and use
                  ;; supplied SPEC unmodified.
                  (transform-spec/full
                   spec)

                  ;; A function - call it.
                  (function
                   (funcall spec bag name meta-data)))))
              (when class-name
                (apply #'rsbag.transform:make-transform class-name args)))
        (continue (&optional condition)
          :report (lambda (stream)
                    (format stream "~@<Do not transform events in ~
                                    channel ~A.~@:>"
                            name))
          (declare (ignore condition)))
        (use-value (transform)
          :report (lambda (stream)
                    (format stream "~@<Use the supplied transform for ~
                                    events in channel ~S.~@:>"
                            name))
          transform))))
