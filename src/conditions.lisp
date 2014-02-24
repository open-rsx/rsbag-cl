;;;; conditions.lisp --- Conditions used in the cl-rsbag system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(define-condition rsbag-condition (condition)
  ()
  (:documentation
   "This condition class is intended to be mixed into all
    rsbag-related condition classes."))

(define-condition rsbag-problem-condition (rsbag-condition)
  ()
  (:documentation
   "This condition class is intended to be mixed into all
    rsbag-related problem (e.g. warning and error) condition
    classes."))

(define-condition rsbag-error (error
                               rsbag-problem-condition)
  ()
  (:documentation
   "This condition class is intended to be mixed into all
    rsbag-related error condition classes."))

(define-condition open-error (rsbag-error
                              chainable-condition)
  ((source :initarg  :source
           :reader   open-error-source
           :documentation
           "Stores the source from which the bag could not be
            opened."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to open bag in source ~
                     ~A.~/more-conditions:maybe-print-cause/~@:>"
             (open-error-source condition)
             condition)))
  (:default-initargs
   :source (missing-required-initarg 'open-error :source))
  (:documentation
   "This error is signaled if a bag, stored in a specific source,
    cannot be opened."))

(define-condition bag-condition (rsbag-condition)
  ((bag :initarg  :bag
        :reader   bag-condition-bag
        :documentation
        "The bag instance in the context of which the error
         occurred."))
  (:default-initargs
   :bag (missing-required-initarg 'bag-condition :bag))
  (:documentation
   "Subclasses of this class are signaled when an error can be
    associated to a specific bag."))

(define-condition no-such-channel (rsbag-error
                                   bag-condition)
  ((name :initarg  :name
         :type     string
         :reader   no-such-channel-name
         :documentation
         "Stores the name of the channel that has been requested."))
  (:report
   (lambda (condition stream)
     (format stream "~@<There is no channel named ~S in the bag ~
                     ~A~@:>"
             (no-such-channel-name condition)
             (bag-condition-bag    condition))))
  (:default-initargs
   :name (missing-required-initarg 'no-such-channel :name))
  (:documentation
   "This error is signaled when a requested channel does not exist
    within a bag an cannot or may not be created."))

(define-condition direction-error (rsbag-error
                                   bag-condition)
  ((expected-direction :initarg  :expected-direction
                       :type     (or direction cons)
                       :reader   direction-error-expected-direction
                       :documentation
                       "Stores the expected direction or
                        directions."))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o
              (bag                bag-condition-bag)
              (expected-direction direction-error-expected-direction))
             condition)
            (actual-direction (bag-direction bag)))
       (format stream "~@<The bag ~A has not been opened with ~
                       direction ~A (but ~A).~@:>"
               bag expected-direction actual-direction))))
  (:default-initargs
   :expected-direction (missing-required-initarg
                        'direction-error :expected-direction))
  (:documentation
   "This error is signaled when an attempt is made to access a bag
    with a direction for which it has not been opened."))

(define-condition channel-condition (bag-condition)
  ((channel :initarg  :channel
            :reader   channel-condition-channel
            :documentation
            "Stores the channel in the context of which the error
             occurred."))
  (:default-initargs
   :channel (missing-required-initarg 'channel-condition :channel))
  (:documentation
   "Subclasses of this class are signaled when an error can be
    associated to a specific channel within a bag."))

(define-condition channel-open-error (rsbag-error
                                      channel-condition
                                      chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not open channel ~S in bag
                     ~A~/more-conditions:maybe-print-cause/~@:>"
             (channel-condition-channel condition)
             (bag-condition-bag         condition)
             condition)))
  (:documentation
   "This error is signaled when an existing channel cannot be
    opened."))

(define-condition channel-exists (rsbag-error
                                  channel-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The channel ~A already exists in bag ~A.~@:>"
             (channel-condition-channel condition)
             (bag-condition-bag         condition))))
  (:documentation
   "This error is signaled when a channel cannot be created because it
    already exists."))

(define-condition no-such-entry (rsbag-error
                                 channel-condition)
  ((key :initarg  :key
        :reader   no-such-entry-key
        :documentation
        "Stores the key for which no entry could be found in the
         channel and bag in question."))
  (:report
   (lambda (condition stream)
     (format stream "~@<No entry could be found for key ~S in the ~
                     channel ~A of bag ~A~@:>"
             (bag-condition-bag         condition)
             (channel-condition-channel condition)
             (no-such-entry-key         condition))))
  (:default-initargs
   :key (missing-required-initarg 'no-such-entry :key))
  (:documentation
   "This error is signaled if a specified entry cannot be found in a
    channel."))
