;;; multi-version.lisp --- Load multiple versions of packages.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsbag.transform)

(defmacro define-serialization-version (version &key versioned?)
  "Define a serialization version VERSION."
  (bind (((:flet versioned-symbol (name package))
	  (find-symbol name (find-package (make-versioned-name
					   package version))))
	(designator (make-versioned-name :rsb-event version)))
   `(progn
      (unless (member ,designator *serialization-versions*)
	(appendf *serialization-versions* '(,designator)))

      (defmethod make-transform ((spec (eql ,designator))
				 &rest args)
	"Handle ARGS appropriately."
	(make-instance (,(versioned-symbol
			  "FIND-TRANSFORM-CLASS" :rsbag.transform)
			 ,(if versioned?
			      (format-symbol :keyword "RSB-EVENT-~A" version)
			      :rsb-event))
		       :wire-schema (first args))

	(bind (((wire-schema &rest rest) args)
	       ((:plist converter) rest))
	  (apply #'make-instance
		 (if converter
		     ',(versioned-symbol
			"RSB-EVENT/PAYLOAD-CONVERSION" :rsbag.transform)
		     ',(versioned-symbol
			"RSB-EVENT"                    :rsbag.transform))
		 :wire-schema wire-schema
		 (when converter
		   (list :converter converter)))))

      (defmethod decode ((transform ,(versioned-symbol
				      "RSB-EVENT" :rsbag.transform))
			 (data      t))
	(,(versioned-symbol "DECODE" :rsbag.transform)
	  transform data))

      (defmethod encode ((transform     ,(versioned-symbol
					  "RSB-EVENT" :rsbag.transform))
			 (domain-object t))
	(,(versioned-symbol "ENCODE" :rsbag.transform)
	  transform domain-object)))))


;;; 0.6 Version
;;

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.6"
			    :rsb.protocol
			    :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
				     :cl-rsbag "compat/0.6/"))
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
		:cl-rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.6" :versioned? t)


;;; 0.5 Version
;;

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.5"
			    :rsb.serialization
			    :rsb.protocol
			    :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
				     :cl-rsbag "compat/0.5/"))
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
		:cl-rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.5")


;;; 0.4 Version
;;

(eval-when (:compile-toplevel :load-toplevel)
  (with-versioned-packages ("0.4"
			    :rsb.serialization
			    :rsb.protocol
			    :rsbag.transform)
    (with-compilation-unit ()
      (let* ((path                  (asdf:system-relative-pathname
				     :cl-rsbag "compat/0.4/"))
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
		:cl-rsbag '("rsb-serialization" "rsb-event-payload-conversion"))))))))

(define-serialization-version "0.4")
