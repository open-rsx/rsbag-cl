;;;; construction.lisp --- Construction of channel <-> events replay connections.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

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
              (incompatible-arguments 'source source name value)))))
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
         (strategy (apply #'make-strategy replay-strategy other-args)))
    (apply #'make-instance 'replay-bag-connection
           :bag      source
           :channels connections
           :strategy strategy
           (when error-policy-supplied?
             (list :error-policy error-policy)))))

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

;;; Utility functions

;;; Remove characters from NAME which would be illegal in scope names.
(defun %legalize-name (name)
  (if-let ((colon-index (position #\: name)))
    (%legalize-name (subseq name 0 colon-index))
    (remove-if (complement (disjoin #'alphanumericp
                                    (curry #'char-equal #\/)))
               name)))

;;; Return two values:
;;;
;;; 1. An URI that is the result of merging CHANNEL-NAME and
;;;    BASE-URI. In the returned URI, normalize the path component of
;;;    BASE-URI and preserve query component of BASE-URI, if present.
;;;
;;; 2. A `scope' or `nil' that is the prefix-`scope' implied by
;;;    BASE-URI. `nil' is returned when BASE-URI does not imply prefix
;;;    scope.
(defun %make-playback-uri (channel-name base-uri)
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

;;; Return a transform object which adds SCOPE as a prefix scope to
;;; the scopes of transformed events if TYPE is of the form
;;;
;;;   (RSB-EVENT* WIRE-SCHEMA)
;;;
;;; .
(defun %make-scope-transform (scope type)
  (when (and scope
             (typep type '(cons keyword))
             (starts-with-subseq "RSB-EVENT" (symbol-name (first type))))
    (lambda (event)
      (setf (event-scope event)
            (rsb:merge-scopes (event-scope event) scope))
      event)))
