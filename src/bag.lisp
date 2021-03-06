;;;; bag.lisp --- The bag class represent data channels stored in a file.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defclass bag (print-items:print-items-mixin)
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
  (let+ (((&structure-r/o
           bag- (backend %backend) (transform %transform) (channels %channels))
          instance)
         ((&flet make-transform (name meta-data id)
            (%make-channel-transform instance name meta-data
                                     :id   id
                                     :spec transform))))
    (iter (for (id name meta-data) in (rsbag.backend:get-channels backend))
          (setf (gethash name channels)
                (%make-channel instance name meta-data
                               (make-transform name meta-data id)
                               :id id)))))

(defmethod print-items:print-items append ((object bag))
  (let+ (((&structure-r/o bag- location direction (channels %channels))
          object)
         (channel-count (hash-table-count channels)))
    `((:location      ,location      "~/rsbag:print-location/")
      (:direction     ,direction     " ~/rsbag:print-direction/" ((:after :location)))
      (:channel-count ,channel-count " (~:D)"                    ((:after :direction))))))

(defmethod close ((bag bag)
                  &key &allow-other-keys)
  (close (bag-%backend bag)))

(defmethod bag-location ((bag bag))
  (rsbag.backend:backend-location (bag-%backend bag)))

(defmethod bag-channels ((bag bag))
  (hash-table-values (bag-%channels bag)))


(flet ((add-channel (bag name meta-data transform transform-supplied?)
         ;; If META-DATA does not have a type, but TRANSFORM is
         ;; non-nil, augment the meta-data with TRANSFORM's type. Make
         ;; a channel instance and store it.
         (let+ (((&structure-r/o bag- (channels %channels) (backend %backend))
                 bag)
                (transform (if transform-supplied?
                               transform
                               (%make-channel-transform
                                bag name meta-data :spec (bag-transform bag))))
                (meta-data (if (and transform (not (getf meta-data :type)))
                               (let ((name (rsbag.transform:transform-name
                                            transform)))
                                 (list* :type name meta-data))
                               meta-data))
                (channel   (%make-channel bag name meta-data transform)))
           (rsbag.backend:put-channel
            backend (channel-%id channel) name meta-data)
           (setf (gethash name channels) channel))))

  (defmethod bag-channel ((bag bag) (name string)
                          &key
                          (if-does-not-exist #'error))
    (or (gethash name (bag-%channels bag))
        (error-behavior-restart-case
            (if-does-not-exist (no-such-channel :bag bag :name name))
          (create (meta-data &key (transform nil transform-supplied?))
            (add-channel bag name meta-data transform transform-supplied?)))))

  (defmethod (setf bag-channel) ((new-value list)
                                 (bag       bag)
                                 (name      string)
                                 &key
                                 (if-exists :error)
                                 (transform nil    transform-supplied?))
    ;; If a channel named NAME already exists, apply IF-EXISTS policy.
    (when-let ((channel (gethash name (bag-%channels bag))))
      (ecase if-exists
        (:error     (error 'channel-exists
                           :bag     bag
                           :channel channel))
        (:supersede (error "Superseding not implemented")))) ; TODO(jmoringe): implement

    (add-channel bag name new-value transform transform-supplied?)))

(defmethod (setf bag-channel) :before ((new-value t)
                                       (bag       t)
                                       (name      t)
                                       &key &allow-other-keys)
  (unless (member (bag-direction bag) '(member :output :io))
    (error 'direction-error
           :bag                bag
           :expected-direction '(member :output :io))))

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
  (let+ (((&structure-r/o bag- (backend %backend) channel-class) bag))
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
