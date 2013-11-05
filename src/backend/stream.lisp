(cl:in-package #:rsbag.backend)

;;; writable-mixin

(defclass writable-mixin ()
  ((write-stream :initarg  :write-stream
                 :type     stream
                 :reader   stream-write-stream
                 :documentation
                 "")
   (executor     :reader   stream-%executor
                 :initform (make-instance 'rsbag::serial-executor)
                 :documentation
                 ""))
  (:default-initargs
   :write-stream (missing-required-initarg 'writable-mixin :write-stream))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod close ((stream writable-mixin) &key abort)
  (unless abort
    (rsbag::dispose (stream-%executor stream)))
  (close (stream-write-stream stream) :abort abort))

(defmethod write-buffer ((backend writable-mixin)
                         (buffer  function))

  (let+ (((&accessors-r/o (stream   stream-write-stream)
                          (executor stream-%executor)) backend)
         (promise (lparallel:promise)))
    (rsbag::executor-submit executor (lambda () (lparallel:fulfill promise (funcall buffer stream))))
    promise))

(defmacro foo-write ((stream stream2 &key (wait? t)) &body body)
  (let ((submit `(write-buffer ,stream2 (lambda (,stream) ,@body))))
   (case wait?
     ((nil) submit)
     ((t)   `(lparallel:force ,submit))
     (t     (let+ (((&with-gensyms promise)))
              `(let ((,promise ,submit))
                 (when ,wait?
                   (lparallel:force ,promise))))))))

;;; io-stream

(defclass io-stream (writable-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defparameter *executed* (lparallel.counter:make-counter))

(time
 (with-output-to-file (stream "/tmp/foo" :if-exists :supersede)
   (let* ((w (make-instance 'io-stream :write-stream stream))
          (tr (iter:iter (iter:for i :to 2)
                         (let ((i* i))
                           (iter:collect (bt:make-thread (lambda ()
                                                           (iter:iter (iter:for i :to 20000)
                                                                      (let ((j i))
                                                                        (write-buffer w (lambda (stream)
                                                                                          (format stream "~D ~D~%" i* j))))))
                                                         :name (princ-to-string i)))))))
     (mapc #'bt:join-thread tr)
     (close w))))
