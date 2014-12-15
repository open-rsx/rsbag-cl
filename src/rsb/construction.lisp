;;;; construction.lisp --- Construction of channel <-> events connections.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb)

;;; RSB events -> bag

(defmethod events->bag ((source listener)
                        (dest   bag)
                        &key
                        (timestamp        :send)
                        (channel-strategy :scope-and-type)
                        (start?           t)
                        &allow-other-keys)
  (make-instance 'recording-channel-connection
                 :bag       dest
                 :endpoint  source
                 :timestamp timestamp
                 :strategy  (apply #'make-channel-strategy
                                   (ensure-list channel-strategy))
                 :start?    start?))

(defmethod events->bag ((source puri:uri)
                        (dest   bag)
                        &rest args
                        &key
                        (transports '((t :expose (:rsb.transport.wire-schema)
                                         &inherit)))
                        (filters    nil filters-supplied?))
  (let ((listener (apply #'make-participant :listener source
                         :transports transports
                         :converters '((t . :fundamental-null))
                         (when filters-supplied?
                           (list :filters filters)))))
    (apply #'events->bag listener dest
           (remove-from-plist args :transports :filters))))

(defmethod events->bag ((source string)
                        (dest   bag)
                        &rest args &key)
  (apply #'events->bag (puri:parse-uri source) dest args))

(defmethod events->bag ((source sequence)
                        (dest   bag)
                        &rest
                        args
                        &key
                        (error-policy nil error-policy-supplied?))
  (apply #'make-instance 'bag-connection
         :bag      dest
         :channels (map 'list
                        (lambda (source)
                          (apply #'events->bag source dest
                                 (remove-from-plist args :error-policy)))
                        source)
         (when error-policy-supplied?
           (list :error-policy error-policy))))

(macrolet ((define-open-bag-method (type)
             `(progn
                (defmethod events->bag ((source string)
                                        (dest   ,type)
                                        &rest args &key)
                  (apply #'events->bag (list source) dest args))

                (defmethod events->bag ((source t)
                                        (dest   ,type)
                                        &rest args
                                        &key
                                        (if-exists      :error if-exists-supplied?)
                                        backend
                                        (flush-strategy nil)
                                        (bag-class      'synchronized-bag))
                  (apply #'events->bag source
                         (apply #'open-bag dest
                                :bag-class bag-class
                                :direction :output
                                (append (when if-exists-supplied?
                                          (list :if-exists if-exists))
                                        (when backend
                                          (list :backend backend))
                                        (when flush-strategy
                                          (list :flush-strategy flush-strategy))))
                         (remove-from-plist args :backend :flush-strategy :bag-class))))))
  (define-open-bag-method string)
  (define-open-bag-method pathname)
  (define-open-bag-method stream))

;;; bag -> RSB events

(macrolet ((define-open-bag-method (type)
             `(defmethod bag->events ((source ,type)
                                      (dest   t)
                                      &rest args
                                      &key
                                      backend
                                      transform
                                      (bag-class 'synchronized-bag))
                (apply #'bag->events
                       (apply #'open-bag source
                              :bag-class bag-class
                              :direction :input
                              (append (when backend
                                        (list :backend backend))
                                      (when transform
                                        (list :transform transform))))
                       dest
                       (remove-from-plist
                        args :backend :transform :bag-class)))))
  (define-open-bag-method string)
  (define-open-bag-method pathname)
  (define-open-bag-method stream))

(defmethod bag->events ((source bag)
                        (dest   t)
                        &rest args
                        &key
                        (error-policy    nil error-policy-supplied?)
                        (replay-strategy :recorded-timing)
                        (channels        t)
                        (backend         nil backend-supplied?)
                        (transform       nil transform-supplied?)
                        (bag-class       nil bag-class-supplied?))
  (let+ (((&flet check-arg (name value supplied?)
           (when supplied?
             (more-conditions:incompatible-arguments
              'source source name value)))))
    (check-arg :backend   backend   backend-supplied?)
    (check-arg :transform transform transform-supplied?)
    (check-arg :bag-class bag-class bag-class-supplied?))

  (let+ ((predicate  (if (eq channels t) (constantly t) channels))
         (channels   (remove-if-not predicate (bag-channels source)))
         (other-args (remove-from-plist
                      args :error-policy :replay-strategy :channels))
         ((&flet do-channel (channel)
            (apply #'bag->events channel dest other-args)))
         (connections (map 'list #'do-channel channels))
         ((class &rest args) (ensure-list replay-strategy))
         (strategy (apply #'make-replay-strategy class
                          (append other-args args))))
    (apply #'make-instance 'replay-bag-connection
           :bag      source
           :channels connections
           :strategy strategy
           (when error-policy-supplied?
             (list :error-policy error-policy)))))

(defmethod bag->events ((source t)
                        (dest   string)
                        &rest args &key)
  (apply #'bag->events source (puri:parse-uri dest) args))

(defmethod bag->events ((source channel)
                        (dest   puri:uri)
                        &key)
  (let+ ((name (%legalize-name
                (if (starts-with #\/ (channel-name source))
                    (subseq (channel-name source) 1)
                    (channel-name source))))
         ((&values uri prefix-scope) (%make-playback-uri name dest))
         ((&plist-r/o (type :type)) (channel-meta-data source))
         (converter
          (make-instance 'rsb.converter:force-wire-schema
                         :wire-schema (if (consp type)
                                          (second type)
                                          :bytes)))
         (participant
          (apply #'make-participant :informer uri
                 :converters `((t . ,converter))
                 (when-let ((transform
                             (%make-scope-transform prefix-scope type)))
                   (list :transform (list transform))))))
    (make-instance 'participant-channel-connection
                   :bag      (channel-bag source)
                   :channels (list source)
                   :endpoint participant)))

(defmethod bag->events ((source channel)
                        (dest   function)
                        &key)
  (make-instance 'channel-connection
                 :bag      (channel-bag source)
                 :channels (list source)
                 :endpoint dest))

;;; Utility functions

(defun %legalize-name (name)
  "Remove characters from NAME which would be illegal in scope names."
  (if-let ((colon-index (position #\: name)))
    (%legalize-name (subseq name 0 colon-index))
    (remove-if (complement (disjoin #'alphanumericp
                                    (curry #'char-equal #\/)))
               name)))

(defun %make-playback-uri (channel-name base-uri)
  "Return two values:

   1. An URI that is the result of merging CHANNEL-NAME and
      BASE-URI. In the returned URI, normalize the path component of
      BASE-URI and preserve query component of BASE-URI, if present.

   2. A `scope' or `nil' that is the prefix-`scope' implied by
      BASE-URI. `nil' is returned when BASE-URI does not imply prefix
      scope."
  (let ((base-uri (puri:copy-uri base-uri)))
    ;; If BASE-URI has a path, ensure it ends with "/" to prevent
    ;; `puri:merge-uri' from acting differently depending on whether
    ;; there is a "/" or not.
    (unless (ends-with #\/ (puri:uri-path base-uri))
      (setf (puri:uri-path base-uri)
            (concatenate 'string (puri:uri-path base-uri) "/")))
    (let ((result (puri:merge-uris channel-name base-uri))
          (prefix (unless (string= (puri:uri-path base-uri) "/")
                    (rsb:make-scope (puri:uri-path base-uri) :intern? t))))
      ;; Merging stomps on the query part, if any, of
      ;; BASE-URI. Restore it afterward.
      (when (puri:uri-query base-uri)
        (setf (puri:uri-query result) (puri:uri-query base-uri)))
      (values result prefix))))

(defun %make-scope-transform (scope type)
  "Return a transform object which adds SCOPE as a prefix scope to the
   scopes of transformed events if TYPE is of the form

     (RSB-EVENT* WIRE-SCHEMA)

   ."
  (when (and scope
             (typep type '(cons keyword))
             (starts-with-subseq "RSB-EVENT" (symbol-name (first type))))
    (lambda (event)
      (setf (event-scope event)
            (rsb:merge-scopes (event-scope event) scope))
      event)))
