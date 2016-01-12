;;;; channel.lisp --- The channel class represents a time-series of homogeneous data.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defclass channel (plist-meta-data-mixin
                   #+sbcl sequence
                   print-items:print-items-mixin)
  ((bag       :initarg  :bag
              :reader   channel-bag
              :documentation
              "Stores the bag instance in which this channel is
               contained.")
   (name      :initarg  :name
              :type     string
              :reader   channel-name
              :documentation
              "Stores the name of the channel.")
   (meta-data :reader   channel-meta-data)
   (transform :initarg  :transform
              :reader   channel-transform
              :initform nil
              :documentation
              "Stores a transformation that should be applied to
               entries when they are retrieved or stored.")
   (id        :initarg  :id
              :reader   channel-%id
              :documentation
              "Stores the id of the channel.")
   (backend   :initarg  :backend
              :reader   channel-%backend
              :documentation
              "Stores a reference to the backend object which
               implements access to the bag to which this channel
               belongs."))
  (:default-initargs
   :bag     (missing-required-initarg 'channel :bag)
   :name    (missing-required-initarg 'channel :name)
   :id      (missing-required-initarg 'channel :id)
   :backend (missing-required-initarg 'channel :backend))
  (:documentation
   "Instances of this class represent time-series of homogeneous data
    items."))

(defmethod print-items:print-items append ((object channel))
  (let+ (((&structure-r/o channel- name transform) object)
         (length (length object))
         (transform (when transform
                      (rsbag.transform:transform-name transform))))

    `((:name      ,name       "~S")
      (:length    ,length   " (~:D)"    ((:after :name)))
      (:transform ,transform "~@[ ~A~]" ((:after :length))))))

(defmethod channel-timestamps/raw ((channel channel))
  (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel))
    (rsbag.backend:get-timestamps backend id)))

(defmethod channel-timestamps ((channel channel))
  (make-instance 'channel-timestamps
                 :timestamps (channel-timestamps/raw channel)))

#+sbcl
(defmethod channel-entries ((channel channel))
  ;; Since CHANNEL is a sequence of its entries, nothing has to be
  ;; done.
  channel)

#+sbcl
(defmethod channel-items ((channel channel))
  ;; Return an instance of `channel-items' which presents pairs of
  ;; timestamps and entries.
  (make-instance 'channel-items :channel channel))

(defmethod entry :around ((channel channel)
                          (index   t)
                          &key
                          (if-does-not-exist nil)
                          (transform         (channel-transform channel)))
  (check-type if-does-not-exist if-does-not-exist-policy)

  (call-next-method channel index
                    :if-does-not-exist if-does-not-exist
                    :transform         transform))

(flet ((return-entry (channel index entry if-does-not-exist transform)
         (let ((raw (or entry
                        (ecase if-does-not-exist
                          (:error (error 'no-such-entry
                                         :bag     (channel-bag channel)
                                         :channel channel
                                         :key     index))
                          ((nil)  nil)))))
           (if transform
               (rsbag.transform:decode transform raw)
               raw))))
  (declare (inline return-entry))

  (defmethod entry ((channel channel)
                    (index   integer)
                    &key if-does-not-exist transform)
    (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel)
           (raw (rsbag.backend:get-entry-at-index backend id index)))
      (return-entry channel index raw if-does-not-exist transform)))

  (defmethod entry ((channel channel)
                    (index   local-time:timestamp)
                    &key
                    if-does-not-exist transform)
    (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel)
           (timestamp (rsbag.backend:timestamp->uint64 index))
           (raw       (rsbag.backend:get-entry-at-time backend id timestamp)))
      (return-entry channel index raw if-does-not-exist transform))))

(defmethod (setf entry) :around ((new-value t)
                                 (channel   channel)
                                 (index     t)
                                 &key
                                 (if-exists :error)
                                 (transform (channel-transform channel)))
  (check-type if-exists if-exists-policy)

  (when (eq if-exists :supersede)
    (error "Superseding entries is not supported yet"))

  (unless (member (bag-direction (channel-bag channel)) '(:output :io))
    (error 'direction-error
           :bag                (channel-bag channel)
           :expected-direction '(member :output :io)))

  (call-next-method new-value channel index
                    :if-exists if-exists
                    :transform transform))

(defmethod (setf entry) ((new-value t)
                         (channel   channel)
                         (index     local-time:timestamp)
                         &key if-exists transform)
  (declare (ignore if-exists))
  (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel)
         (timestamp (rsbag.backend:timestamp->uint64 index))
         (raw       (if transform
                        (rsbag.transform:encode transform new-value)
                        new-value)))
    (rsbag.backend:put-entry backend id timestamp raw)
    new-value))

;;; Time range protocol

(defmethod start-timestamp ((channel channel))
  (unless (emptyp channel)
    (elt (channel-timestamps channel) 0)))

(defmethod end-timestamp ((channel channel))
  (unless (emptyp channel)
    (elt (channel-timestamps channel) (1- (length channel)))))

;;; Sequence protocol

#+sbcl
(defmethod sequence:length ((sequence channel))
  (let+ (((&structure-r/o channel- (id %id) (backend %backend)) sequence))
    (rsbag.backend:get-num-entries backend id)))

#+sbcl
(defmethod sequence:make-sequence-like ((sequence channel)
                                        (length   integer)
                                        &rest args
                                        &key
                                        initial-element
                                        initial-contents)
  (declare (ignore initial-element initial-contents))
  (apply #'make-array length args))

#+sbcl
(defmethod sequence:elt ((sequence channel) (index integer))
  (entry sequence index))

;;; `channel-timestamps' sequence class

#+sbcl
(defclass channel-timestamps (standard-object
                              sequence)
  ((timestamps :initarg  :timestamps
               :accessor %timestamps
               :documentation
               "Stores the sequence of associated timestamps for the
                entries of the channel."))
  (:default-initargs
   :timestamps (missing-required-initarg 'channel-timestamps :timestamps))
  (:documentation
   "A sequence of timestamps for the entries of a channel."))

#+sbcl
(defmethod sequence:length ((sequence channel-timestamps))
  (length (%timestamps sequence)))

#+sbcl
(defmethod sequence:elt ((sequence channel-timestamps)
                         (index    integer))
  (rsbag.backend:uint64->timestamp (elt (%timestamps sequence) index)))

;;; `channel-items' sequence class

#+sbcl
(defclass channel-items (standard-object
                         sequence)
  ((channel    :initarg  :channel
               :reader   %channel
               :documentation
               "Stores the channel the items of which are used.")
   (timestamps :accessor %timestamps
               :documentation
               "Stores the sequence of associated timestamps for the
                entries of the channel."))
  (:default-initargs
   :channel (missing-required-initarg 'channel-items :channel))
  (:documentation
   "Instances of this class can be used to access the timestamps and
    associated entries of a channel."))

#+sbcl
(defmethod shared-initialize :after ((instance   channel-items)
                                     (slot-names t)
                                     &key
                                     channel)
  (setf (%timestamps instance) (channel-timestamps/raw channel)))

#+sbcl
(defmethod sequence:length ((sequence channel-items))
  (length (%channel sequence)))

#+sbcl
(defmethod sequence:elt ((sequence channel-items) (index integer))
  (let+ (((&accessors-r/o (channel %channel) (timestamps %timestamps))
          sequence))
    (list (rsbag.backend:uint64->timestamp (elt timestamps index))
          (elt channel index))))
