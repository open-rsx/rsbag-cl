;;;; threadpool.lisp --- A threadpool for async rsbag operations.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

(defvar *threadpool* nil
  "When non-nil, holds the threadpool used by the rsbag system.")

(defun start-threadpool ()
  "Create and initialize a threadpool for use by the rsbag system."
  (when *threadpool*
    (warn "~@<Threadpool already initialized to ~A.~@:>"
          *threadpool*))
  (setf *threadpool* (lparallel:make-kernel 2 :name "rsbag")))

(defun stop-threadpool ()
  "Stop all threads of the threadpool used by the rsbag system."
  (let ((lparallel:*kernel* *threadpool*))
    (lparallel:end-kernel :wait t))
  (setf *threadpool* nil))

(defmacro with-threadpool (&body body)
  "Execute BODY such that created tasks use the rsbag threadpool and
errors are transferred."
  `(let ((lparallel:*kernel* *threadpool*))
     (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
       ,@body)))

;; Start the threadpool when loading or executing this. It may have to
;; be stopped and restarted when saving an image. See reloading.lisp.
(unless *threadpool*
  (start-threadpool))

;;; Executor

(defclass serial-executor ()
  ((queue   :reader   executor-%queue
            :initform (lparallel.queue:make-queue))
   (channel :reader   executor-%channel
            :initform (with-threadpool
                        (lparallel:make-channel))
            :documentation
            ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   serial-executor)
                                     (slot-names t)
                                     &key)
  (lparallel.queue:push-queue/no-lock
   t (lparallel.kernel::channel-queue (executor-%channel instance))))

(defmethod dispose ((executor serial-executor) &key abort)
  (unless abort
    (let+ (((&accessors-r/o (channel executor-%channel)
                            (queue   executor-%queue)) executor))
      (with-threadpool
        (iter (for token next (lparallel:receive-result channel))
              (until (lparallel.queue:queue-empty-p queue))
              (lparallel.queue:push-queue
               token (lparallel.kernel::channel-queue channel)))))))

(defmethod executor-submit ((executor serial-executor)
                            (task     function))
  (with-threadpool
    (let+ (((&accessors-r/o (queue   executor-%queue)
                            (channel executor-%channel)) executor)
           (empty? (lparallel.queue:with-locked-queue queue
                     (prog1
                         (lparallel.queue:queue-empty-p/no-lock queue)
                       (lparallel.queue:push-queue/no-lock task queue))))
           ((&labels work ()
              (let+ ((token (lparallel:receive-result channel))
                     ((&values task empty?)
                      (lparallel.queue:with-locked-queue queue
                        (values
                         (lparallel.queue:pop-queue/no-lock queue)
                         (lparallel.queue:queue-empty-p/no-lock queue)))))
                (funcall task)
                (unless empty?
                  (lparallel:submit-task channel #'work))
                token))))
      (when empty?
        (lparallel:submit-task channel #'work))
      (values))))
