;;;; interactive.lisp --- A strategy for interactive replay control.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.rsb.replay)

(defclass interactive (error-policy-mixin
                       filtering-mixin
                       external-driver-mixin
                       timestamp-adjustment-mixin
                       print-items:print-items-mixin)
  ((stream           :initarg  :stream
                     :type     stream
                     :accessor strategy-stream
                     :initform *query-io*
                     :documentation
                     "Stores the bi-directional stream that is used
                      for user interaction.")
   (previous-command :initarg  :previous-command
                     :type     (or null function)
                     :accessor strategy-previous-command
                     :initform nil
                     :documentation
                     "Stores the previously invoked command or nil."))
  (:default-initargs
   :error-policy #'continue)
  (:documentation
   "This strategy allows controlling the replay process interactively
    by means of textual commands."))

(service-provider:register-provider/class
 'replay-strategy :interactive :class 'interactive)

(defmethod next-command ((strategy interactive))
  (let+ (((&structure-r/o strategy- commands stream) strategy)
         ((&accessors (previous strategy-previous-command)) strategy))

    ;; Read commands an existing one is specified.
    (iter (for (name . args) next (%read-command stream))
          (for command       next (if name
                                      (find-command strategy name
                                                    :test   #'starts-with-subseq
                                                    :error? nil)
                                      previous))
          (if command
              (progn
                (setf previous command)
                (return (lambda ()
                          (when-let ((result (multiple-value-list
                                              (apply command args))))
                            (format stream "~A~%" (first result))))))
              (format stream "~@<~:[Cannot repeat command without ~
                              previous command~;~:*No such command: ~
                              ~S~]. Available commands: ~{~(~A~)~^, ~
                              ~}.~@:>~%"
                      name (mapcar #'car commands))))))

(defmethod execute-command ((strategy interactive)
                            (command  function))
  (funcall command))

(defmethod replay ((connection replay-bag-connection)
                   (strategy   interactive)
                   &key &allow-other-keys)
  (let+ (((&structure-r/o strategy- stream num-repetitions) strategy))
    (format stream "~&~@<OHAI, type command; unambiguous prefix ~
                    suffices. empty command repeats previous ~
                    one.~@:>~%")
    ;; Try to execute specified commands and print error reports if
    ;; something goes wrong. Note that no control transfer happens
    ;; here. That is done via the restarts established by
    ;; `replay-restart-mixin' and, potentially, the error policy.
    (handler-bind ((error (lambda (condition)
                            (format stream "~&~@<Error ~A~@:>~%" condition)))) ; TODO put this into the error policy?
      (call-repeatedly num-repetitions (lambda () (call-next-method))))
    (format stream "~&~@<KTHXBYE~@:>~%")))

;;; Utility functions

(defun %read-command (stream)
  (let+ (((&flet line ()
            (format stream "~&> ")
            (force-output stream)
            (read-line stream nil "quit")))
         ((&optional name &rest args)
          (split-sequence #\Space (line)
                          :remove-empty-subseqs t)))
    (when name
      (cons (string-upcase name) (mapcar #'read-from-string args)))))
