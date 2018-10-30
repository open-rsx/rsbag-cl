;;;; multi-version.lisp --- Load multiple versions of packages.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.transform)

(defmacro define-serialization-version (version
                                        &key
                                        current?)
  "Define a serialization version VERSION."
  (let+ (((&flet versioned-symbol (name package)
            (let ((package (find-package
                            (if current?
                                '#:rsbag.transform
                                (make-versioned-name package version)))))
              (find-symbol (string name) package))))
         ((designator designator/payload-conversion)
          (mapcar (rcurry #'make-versioned-name version)
                  #1='(#:rsb-event #:rsb-event/payload-conversion)))
         ((class-name class/payload-conversion-name)
          (mapcar (rcurry #'versioned-symbol :rsbag.transform) #1#)))
    `(progn
       (unless (member ,designator *serialization-versions*)
         (appendf *serialization-versions* '(,designator)))

       (defmethod make-transform ((spec (eql ,designator)) &rest args)
         ;; Handle ARGS appropriately.
         (check-type args (cons keyword list)
                     "a wire-schema keyword, optionally followed by keyword arguments")

         (let+ (((wire-schema &rest rest &key converter) args))
           (apply #'service-provider:make-provider
                  'transform
                  (if converter
                      ,designator/payload-conversion
                      ,designator)
                  :wire-schema wire-schema
                  rest)))

       (service-provider:register-provider/class
        'transform ',designator :class ',class-name)

       (service-provider:register-provider/class
        'transform ',designator/payload-conversion
        :class ',class/payload-conversion-name)

       ,@(unless current?
           `((defmethod decode ((transform ,class-name)
                                (data      t))
               (,(versioned-symbol "DECODE" :rsbag.transform)
                 transform data))

             (defmethod encode ((transform     ,class-name)
                                (domain-object t))
               (,(versioned-symbol "ENCODE" :rsbag.transform)
                 transform domain-object)))))))

;;; Current Version

(define-serialization-version
  #.(format nil "~{~D~^.~}" (rsbag-system:serialization-version/list))
  :current? t)

;;; 0.8 Version

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.8"
                            :rsb.protocol
                            :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
                                     :rsbag "compat/0.8/"))
             (pbf:*proto-load-path* (cons (merge-pathnames "data/" path)
                                          pbf:*proto-load-path*)))
        ;; Load relevant protocol buffer types.
        (iter (for file in '("data/rsb/protocol/EventId.proto"
                             "data/rsb/protocol/EventMetaData.proto"
                             "data/rsb/protocol/Notification.proto"))
              (map nil (curry #'pbb:emit (pbf:load/text (merge-pathnames file path)))
                   '(:class :packed-size :serializer :deserializer)))
        ;; Load implementation.
        (map nil (compose #'load (rcurry #'merge-pathnames path))
             '("src/transform/package.lisp"
               "src/transform/protocol.lisp"
               "src/transform/conditions.lisp"
               "src/transform/rsb-event.lisp"))
        ;; Inject the version of rsb-event that supports payload
        ;; conversion.
        (load (asdf:component-pathname
               (asdf:find-component
                :rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.8")

;;; 0.7 Version

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.7"
                            :rsb.protocol
                            :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
                                     :rsbag "compat/0.7/"))
             (pbf:*proto-load-path* (cons (merge-pathnames "data/" path)
                                          pbf:*proto-load-path*)))
        ;; Load relevant protocol buffer types.
        (iter (for file in '("data/rsb/protocol/EventId.proto"
                             "data/rsb/protocol/EventMetaData.proto"
                             "data/rsb/protocol/Notification.proto"))
              (map nil (curry #'pbb:emit (pbf:load/text (merge-pathnames file path)))
                   '(:class :packed-size :serializer :deserializer)))
        ;; Load implementation.
        (map nil (compose #'load (rcurry #'merge-pathnames path))
             '("src/transform/package.lisp"
               "src/transform/protocol.lisp"
               "src/transform/conditions.lisp"
               "src/transform/rsb-event.lisp"))
        ;; Inject the version of rsb-event that supports payload
        ;; conversion.
        (load (asdf:component-pathname
               (asdf:find-component
                :rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.7")

;;; 0.6 Version

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.6"
                            :rsb.protocol
                            :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
                                     :rsbag "compat/0.6/"))
             (pbf:*proto-load-path* (cons (merge-pathnames "data/" path)
                                          pbf:*proto-load-path*)))
        ;; Load relevant protocol buffer types.
        (iter (for file in '("data/rsb/protocol/EventId.proto"
                             "data/rsb/protocol/EventMetaData.proto"
                             "data/rsb/protocol/Notification.proto"))
              (map nil (curry #'pbb:emit (pbf:load/text (merge-pathnames file path)))
                   '(:class :packed-size :serializer :deserializer)))
        ;; Load implementation.
        (map nil (compose #'load (rcurry #'merge-pathnames path))
             '("src/transform/package.lisp"
               "src/transform/protocol.lisp"
               "src/transform/conditions.lisp"
               "src/transform/rsb-event.lisp"))
        ;; Inject the version of rsb-event that supports payload
        ;; conversion.
        (load (asdf:component-pathname
               (asdf:find-component
                :rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.6")

;;; 0.5 Version

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.5"
                            :rsb.serialization
                            :rsb.protocol
                            :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
                                     :rsbag "compat/0.5/"))
             (pbf:*proto-load-path* (cons (merge-pathnames "data/" path)
                                          pbf:*proto-load-path*)))
        ;; Load relevant protocol buffer types.
        (iter (for file in '("data/rsb/protocol/MetaData.proto"
                             "data/rsb/serialization/Event.proto"))
              (map nil (curry #'pbb:emit (pbf:load/text (merge-pathnames file path)))
                   '(:class :packed-size :serializer :deserializer)))
        ;; Load implementation.
        (map nil (compose #'load (rcurry #'merge-pathnames path))
             '("src/transform/package"
               "src/transform/protocol"
               "src/transform/conditions"
               "src/transform/rsb-event"))
        ;; Inject the version of rsb-event that supports payload
        ;; conversion.
        (load (asdf:component-pathname
               (asdf:find-component
                :rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.5")

;;; 0.4 Version

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.4"
                            :rsb.serialization
                            :rsb.protocol
                            :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
                                     :rsbag "compat/0.4/"))
             (pbf:*proto-load-path* (cons (merge-pathnames "data/" path)
                                          pbf:*proto-load-path*)))
        ;; Load relevant protocol buffer types.
        (iter (for file in '("data/rsb/protocol/MetaData.proto"
                             "data/rsb/serialization/Event.proto"))
              (map nil (curry #'pbb:emit (pbf:load/text (merge-pathnames file path)))
                   '(:class :packed-size :serializer :deserializer)))
        ;; Load implementation.
        (map nil (compose #'load (rcurry #'merge-pathnames path))
             '("src/transform/package"
               "src/transform/protocol"
               "src/transform/rsb-event"))
        ;; Inject the version of rsb-event that supports payload
        ;; conversion.
        (load (asdf:component-pathname
               (asdf:find-component
                :rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.4")
