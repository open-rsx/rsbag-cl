;;;; protocol.lisp --- Protocol used by the cl-rsbag system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

;;; Bag opening protocol

(defgeneric open-bag (source
                      &rest args
                      &key
                      direction
                      if-exists
                      backend
                      flush-strategy
                      bag-class
                      transform
                      &allow-other-keys)
  (:documentation
   "Open the data source SOURCE and return a bag object.

    BACKEND designates the backend that should be used and
    ARGS (except the keyword arguments :backend, :bag-class and
    :transform) are passed to the backend.

    DIRECTION can be any of :input, :output and :io.

    IF-EXISTS specifies the behavior when DIRECTION is :output or :io and
    SOURCE already exists. Valid values are :error and :overwrite. See
    `cl:open' for more information.

    BACKEND has to be a keyword naming a backend class. Available
    backends can be inspected using the service designated by
    `rsbag.backend:backend'.

    When supplied, BAG-CLASS specifies the class of which the returned bag
    object should be an instance.

    When supplied, TRANSFORM specifies a transformation that should be
    applied to all entries read from or written to the returned bag
    object. See type `transform-spec'.

    Example:
    RSBAG> (open-bag #p\"/tmp/mylog.tide\" :direction :input)
    #<BAG \"mylog.tide\" r- (1) {100A2ECDF3}>"))

;; Default behavior

(defmethod open-bag :around ((source t)
                             &rest args &key)
  (with-condition-translation
      (((error open-error)
        :source source))
    (iter (restart-case
              (return (apply #'call-next-method source args))
            (retry ()
              :report (lambda (stream)
                        (format stream "~@<Retry opening the bag ~
                                        stored in ~S.~@:>"
                                source)))
            (use-source (new-value)
              :report      (lambda (stream)
                             (format stream "~@<Use a different source ~
                                             instead of ~S.~@:>"
                                     source))
              :interactive (lambda ()
                             (format *query-io* "~@<Specify ~
                                                 source (not ~
                                                 evaluated): ~@:>")
                             (force-output *query-io*)
                             (list (read *query-io*)))
              (setf source new-value))))))

(defmethod open-bag ((source stream)
                     &rest args
                     &key
                     location
                     (direction      (missing-required-argument :direction))
                     (backend        (missing-required-argument :backend))
                     (flush-strategy nil)
                     (bag-class      'bag)
                     transform)
  (check-type direction direction      "either :input, :output or :io")
  (check-type transform transform-spec "a transformation specification")
  (when (and flush-strategy (eq direction :input))
    (incompatible-arguments :direction      direction
                            :flush-strategy flush-strategy))

  (let ((backend (apply #'rsbag.backend:make-backend backend
                        :stream    source
                        :location  location
                        :direction direction
                        (append
                         (when flush-strategy
                           (list :flush-strategy
                                 (apply #'rsbag.backend:make-flush-strategy
                                        (ensure-list flush-strategy))))
                         (remove-from-plist
                          args :direction :backend :flush-strategy
                               :bag-class :transform)))))
    (make-instance bag-class
                   :backend   backend
                   :direction direction
                   :transform transform)))

(defmethod open-bag ((source pathname)
                     &rest args
                     &key
                     (location  source)
                     (direction :io)
                     (if-exists :error)
                     (backend   nil     backend-supplied?))
  (let* ((backend (cond
                    (backend-supplied?
                     backend)
                    ((not (stringp (pathname-type source)))
                     (error "~@<Cannot guess file format because ~A is ~
                             missing a type component.~@:>"
                            source))
                    (t
                     (make-keyword (string-upcase (pathname-type source))))))
         (stream  (open source
                        :element-type      '(unsigned-byte 8)
                        :direction         direction
                        :if-exists         if-exists
                        :if-does-not-exist (case direction
                                             (:input        :error)
                                             ((:output :io) :create)))))
    (apply #'open-bag stream
           :location  location
           :direction direction
           :backend   backend
           (remove-from-plist
            args :location :direction :if-exists :backend))))

(defmethod open-bag ((source string)
                     &rest args
                     &key &allow-other-keys)
  (apply #'open-bag (parse-namestring source) args))

;;; Bag protocol

(defgeneric bag-location (bag)
  (:documentation
   "Return an object representing the location of BAG.

    Possible values include (but are not limited to) NIL and
    `cl:pathname' objects."))

(defgeneric bag-direction (bag)
  (:documentation
   "Return the direction of BAG.

    One of :input, :output, :io."))

(defgeneric bag-transform (bag)
  (:documentation
   "Return the transform specification associated to BAG.

    The specification is used to make concrete transformations for all
    channels of BAG."))

(defgeneric bag-channels (bag)
  (:documentation
   "Return a list of the `channel's stored in BAG."))

(defgeneric bag-channel (bag name
                         &key
                         if-does-not-exist)
  (:documentation
   "Return the `channel' named NAME in BAG.

    IF-DOES-NOT-EXIST controls the behavior in case the requested
    channel does not exist. If a function designator (like the default
    'error) is supplied, the function is called with an instance of
    the `no-such-channel' condition class. Values of other types are
    returned instead of the missing channel. In case IF-DOES-NOT-EXIST
    is a function designator, a `create' restart with lambda list

      (meta-data &key transform)

    is established for the duration of the call to
    IF-DOES-NOT-EXIST."))

(defgeneric (setf bag-channel) (spec bag name
                                &key
                                if-exists
                                transform)
  (:documentation
   "Add or update and return the channel named NAME in BAG.

    SPEC is a plist which specifies properties of the created or
    updated channel.

    IF-EXISTS controls the behavior in case a channel named NAME
    already exists in BAG. Valid values are :error, which causes an
    error to be signaled, and :supersede, which causes the existing
    channel to be updated.

    TRANSFORM can be used to specify a transformation that should be
    applied to all value read from/written to the channel. Valid
    values are nil or an object implementing the transform protocol
    specified in rsbag.transform."))

(defgeneric bag-channel-class (bag)
  (:documentation
   "Return the channel class used by BAG."))

;;; Bag behind-the-scenes protocol ;)

(defgeneric %make-channel (bag name meta-data transform
                           &rest args
                           &key
                           id
                           &allow-other-keys)
  (:documentation
   "Create and return a new channel named NAME with id ID and
    associated meta-data META-DATA and TRANSFORM for BAG.

    The returned object implements the channel protocol.

    TRANSFORM can be nil in which case raw data from the underlying
    source is used.

    ARGS are passed to the constructed channel."))

(defgeneric %make-channel-transform (bag name meta-data
                                     &key
                                     id
                                     spec)
  (:documentation
   "Make and return a suitable transformation for the channel in BAG
    described by NAME, META-DATA, ID and SPEC.

    SPEC can be used to specify additional parameters for the
    constructed transformation or to specify an entirely different
    transformation. See the type `transform-spec'."))

;;; Channel protocol

(defgeneric channel-bag (channel)
  (:documentation
   "Return the bag containing CHANNEL."))

(defgeneric channel-name (channel)
  (:documentation
   "Return the name of CHANNEL."))

(defgeneric channel-transform (channel)
  (:documentation
   "Return the transformation applied to each datum retrieved from
    CHANNEL."))

(defgeneric channel-meta-data (channel)
  (:documentation
   "Return the meta-data associated to CHANNEL as plist."))

(defgeneric channel-timestamps (channel)
  (:documentation
   "Return a sequence of `local-time:timestamp' objects representing
    the points in time for which CHANNEL contains entries."))

(defgeneric channel-entries (channel)
  (:documentation
   "Return a sequence of the entries of CHANNEL."))

(defgeneric channel-items (channel)
  (:documentation
   "Return a sequence of elements of the form (TIMESTAMP ENTRY) for
    the timestamps and associated entries of CHANNEL."))

(defgeneric entry (channel index
                   &key
                   if-does-not-exist)
  (:documentation
   "Return the entry at position or time INDEX in CHANNEL.

    If INDEX is an integer, the INDEX-th entry is returned. If INDEX
    is a `local-time:timestamp' instances, the entry stored for the
    point in time represented by INDEX is returned.

    IF-DOES-NOT-EXIST controls the behavior in case there is no entry
    for INDEX. Valid values are nil, which causes nil to be returned
    and :error, which causes an error to be signaled."))

(defgeneric (setf entry) (new-value channel index
                          &key
                          if-exists)
  (:documentation
   "Store NEW-VALUE as the value of the entry at position or time
    INDEX in CHANNEL.

    If INDEX is an integer, NEW-VALUE is stored as the INDEX-th
    entry. If INDEX is a `local-time:timestamp' instances, the
    NEW-VALUE is stored for the point in time represented by INDEX.

    IF-EXISTS controls the behavior in case an entry is already stored
    at INDEX. Valid values are :error, which causes an error to be
    signaled, and :supersede, which causes the stored value to be
    replaced."))

;; When available, channels additionally implement the sequence
;; protocol such that the channel appears as a sequence of its
;; entries.

;;; Time range protocol
;;;
;;; Applicable to at least channels and whole bags.

(defgeneric start-timestamp (bag-or-channel)
  (:documentation
   "Return the earliest timestamp for which an entry exists in
    BAG-OR-CHANNEL."))

(defgeneric end-timestamp (bag-or-channel)
  (:documentation
   "Return the earliest timestamp for which an entry exists in
    BAG-OR-CHANNEL."))
