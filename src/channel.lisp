;;;; channel.lisp --- The channel class represents a time-series of homogeneous data.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

(defmethod channel-timestamps ((channel channel))
  (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel))
    (rsbag.backend:get-timestamps backend id)))

#+sbcl
(defmethod channel-entries ((channel channel))
  "Since CHANNEL is a sequence of its entries, nothing has to be done."
  channel)

#+sbcl
(defmethod channel-items ((channel channel))
  "Return an instance of `channel-items' which presents pairs of
   timestamps and entries."
  (make-instance 'channel-items
                 :channel channel))

;; TODO(jmoringe, 2011-12-02): entry methods are almost identical
(defmethod entry ((channel channel)
                  (index   integer)
                  &key
                  if-does-not-exist
                  (transform        (channel-transform channel)))
  (check-type if-does-not-exist if-does-not-exist-policy)

  (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel)
         (raw (or (rsbag.backend:get-entry backend id index)
                  (ecase if-does-not-exist
                    (:error (error 'no-such-entry
                                   :bag     (channel-bag channel)
                                   :channel channel
                                   :key     index))
                    ((nil)  nil)))))
    (if transform
        (rsbag.transform:decode transform raw)
        raw)))

(defmethod entry ((channel   channel)
                  (timestamp local-time:timestamp)
                  &key
                  if-does-not-exist)
  (check-type if-does-not-exist if-does-not-exist-policy)

  (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel))
    (or (rsbag.backend:get-entry backend id timestamp)
        (ecase if-does-not-exist
          (:error (error 'no-such-entry
                         :bag     (channel-bag channel)
                         :channel channel
                         :key     timestamp))
          ((nil)  nil)))))

(defmethod (setf entry) :before ((new-value t)
                                 (channel   channel)
                                 (index     t)
                                 &key &allow-other-keys)
  (unless (member (bag-direction (channel-bag channel)) '(:output :io))
    (error 'direction-error
           :bag                (channel-bag channel)
           :expected-direction '(member :output :io))))

(defmethod (setf entry) ((new-value t)
                         (channel   channel)
                         (index     local-time:timestamp)
                         &key
                         (if-exists :error)
                         (transform (channel-transform channel)))
  (check-type if-exists if-exists-policy)

  (when (eq if-exists :supersede)
    (error "Superseding entries is not supported yet"))

  (let+ (((&structure-r/o channel- (id %id) (backend %backend)) channel)
         (raw (if transform
                  (rsbag.transform:encode transform new-value)
                  new-value)))
    (rsbag.backend:put-entry backend id index raw)
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

;;; `channel-items' sequence class

#+sbcl
(defclass channel-items (standard-object
                         sequence)
  ((channel    :initarg  :channel
               :reader   channel-items-%channel
               :documentation
               "Stores the channel the items of which are used.")
   (timestamps :accessor channel-items-%timestamps
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
  (setf (channel-items-%timestamps instance)
        (channel-timestamps channel)))

#+sbcl
(defmethod sequence:length ((sequence channel-items))
  (length (channel-items-%channel sequence)))

#+sbcl
(defmethod sequence:elt ((sequence channel-items) (index integer))
  (let+ (((&structure-r/o
           channel-items- (channel %channel) (timestamps %timestamps))
          sequence))
    (list (elt timestamps index) (elt channel index))))
