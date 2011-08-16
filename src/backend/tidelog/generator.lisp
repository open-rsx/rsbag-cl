;;; generator.lisp --- Generate data-holders and (de)serializers for TIDE log.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsbag.backend.tidelog)


;;; Class generator
;;

(defun specs->class (name specs &key documentation)
  `(defclass ,name ()
     (,@(map 'list (curry #'spec->slot name) specs))
     ,@(when documentation
	     `((:documentation ,documentation)))))

(defun spec->slot (class-name spec)
  (bind (((name type &key documentation) spec)
	 (type          (type-spec->lisp-type type))
	 (accessor-name (symbolicate class-name "-" name)))
    `(,name :initarg  ,(make-keyword name)
	    :type     ,type
	    :accessor ,accessor-name
	    ,@(when documentation
		    `(:documentation
		      ,documentation)))))

(defun type-spec->lisp-type (spec)
  (cond
    ((and (listp spec) (eq (first spec) :string))
     'string)
    ((and (listp spec) (eq (first spec) :repeated))
     'vector)
    ((and (listp spec) (eq (first spec) :blob))
     'binio:octet-vector)
    (t
     spec)))


;;; Size method
;;

(defun specs->size (class-name specs)
  `(defmethod size ((object ,class-name))
     (+ ,@(map 'list (rcurry #'spec->size class-name 'object)
	       specs))))

(defun spec->size (spec class-name object)
  (bind (((name type &rest _) spec)
	 (accessor-name (symbolicate class-name "-" name)))
    (type-spec->size type `(,accessor-name ,object))))

(defun type-spec->size (type-spec value)
  (cond
    ((and (listp type-spec) (keywordp (first type-spec)))
     (destructuring-ecase type-spec
       ((:repeated count-slot sub-type)
	(declare (ignore count-slot))
	`(+ ,(type-spec->size '(unsigned-byte 32) :unused)
	    (iter (for val each ,value)
		  (summing ,(type-spec->size sub-type 'val)))))
       ((:blob length-slot)
	(declare (ignore length-slot))
	`(length ,value))
       ((:string length-type)
	`(+ ,(type-spec->size length-type :unused)
	    (length ,value)))))

    ((and (listp type-spec) (eq (first type-spec) 'unsigned-byte))
     (ash (second type-spec) -3))

    ((symbolp type-spec)
     `(size ,value))))


;;; Deserializer
;;

(defun specs->deserializer (class-name specs)
  `(defmethod unpack ((source simple-array) (object ,class-name)
		      &optional
		      (start 0))
     (check-type source binio:octet-vector)

     (let ((offset start))
       ,@(map 'list (rcurry #'spec->deserializer
			    class-name 'source 'object 'offset)
	      specs)
       (values object (- offset start)))))

(defun spec->deserializer (spec class-name source object offset)
  (bind (((name type &rest _) spec)
	 (accessor-name (symbolicate class-name "-" name)))
    `(bind (((:values value length)
	     ,(type-spec->deserializer type source offset)))
       (declare (type ,(type-spec->lisp-type type) value))
       (setf (,accessor-name ,object) value)
       (incf offset length))))

(defun type-spec->deserializer (type-spec source offset)
  (cond
    ((and (listp type-spec) (keywordp (first type-spec)))
     (destructuring-ecase type-spec
       ((:repeated count-slot sub-type)
	`(iter (repeat (slot-value object ',count-slot)) ;;; TODO(jmoringe): slot access
	       (with offset = ,offset)
	       (bind (((:values value length)
		       ,(type-spec->deserializer sub-type source offset)))
		 (incf offset length)
		 (collect value  :into result :result-type vector)
		 (summing length :into length*))
	       (finally (return (values result length*)))))

       ((:blob length-slot)
	`(let* ((length (slot-value object ',length-slot))) ;;; TODO(jmoringe): slot access
	   (values (subseq ,source ,offset (+ ,offset length)) length)))

       ((:string length-type)
	`(bind (((:values length length-length)
		 ,(type-spec->deserializer length-type source offset))
		(data-offset (+ ,offset length-length)))
	   (values (sb-ext:octets-to-string
		    (subseq source data-offset (+ data-offset length)))
		   (+ length-length length))))))

    ((and (listp type-spec) (eq (first type-spec) 'unsigned-byte))
     (case (second type-spec)
       (8  `(values (aref ,source ,offset) 1))
       (32 `(binio:decode-uint32-le ,source ,offset))
       (64 `(binio:decode-uint64-le ,source ,offset))))

    ((symbolp type-spec)
     `(let ((object (allocate-instance (find-class ',type-spec))))
	(unpack ,source object ,offset)))))


;;; Serializer
;;

(defun specs->serializer (class-name specs)
  `(defmethod pack ((object ,class-name) (source simple-array)
		    &optional
		      (start 0))
     (check-type source binio:octet-vector)

     (let ((offset start))
       ,@(map 'list (rcurry #'spec->serializer
			    class-name 'source 'object 'offset)
	      specs)
       (- offset start))))

(defun spec->serializer (spec class-name source object offset)
  (bind (((name type &rest _) spec)
	 (accessor-name (symbolicate class-name "-" name)))
    `(let ((value (,accessor-name ,object)))
       (declare (type ,(type-spec->lisp-type type) value))
       (incf offset ,(type-spec->serializer type 'value source offset)))))

(defun type-spec->serializer (type-spec value source offset)
  (cond
    ((and (listp type-spec) (keywordp (first type-spec)))
     (destructuring-ecase type-spec
       ((:repeated count-slot sub-type)
	(declare (ignore count-slot))
	`(iter (for val each value)
	       (with offset* = ,offset)
	       (incf offset* ,(type-spec->serializer sub-type 'val source 'offset*))
	       (finally (return (- offset* ,offset)))))

       ((:blob length-slot)
	(declare (ignore length-slot))
	`(progn
	   (setf (subseq ,source ,offset (+ ,offset (length ,value)))
		 value)
	   (length ,value)))

       ((:string length-type)
	`(let ((offset* ,offset))
	   (incf offset* ,(type-spec->serializer
			   length-type `(length ,value) source 'offset*))
	   (incf offset* (binio:encode-utf8 ,value ,source offset*))
	   (- offset* ,offset)))))

    ((and (listp type-spec) (eq (first type-spec) 'unsigned-byte))
     (case (second type-spec)
       (8  `(progn (setf (aref ,source ,offset) ,value) 1))
       (32 `(binio:encode-uint32-le ,value ,source ,offset))
       (64 `(binio:encode-uint64-le ,value ,source ,offset))))

    ((symbolp type-spec)
     `(pack ,value ,source ,offset))))
