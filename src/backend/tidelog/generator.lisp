;;; generator.lisp --- Generate data-holders and (de)serializers for TIDE log.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsbag.backend.tidelog)


;;; Class generator
;;

(defun specs->class (name specs &key documentation)
  `(defclass ,name ()
     (,@(map 'list (curry #'spec->slot name) specs))
     ,@(when documentation
	     `((:documentation ,documentation)))))

(defun spec->slot (class-name spec)
  (let+ (((name type &key documentation) spec)
	 (type          (type-spec->lisp-type type))
	 (accessor-name (symbolicate class-name "-" name)))
    `(,name :initarg  ,(make-keyword name)
	    :type     ,type
	    :accessor ,accessor-name
	    ,@(when documentation
		    `(:documentation ,documentation)))))

(defun type-spec->lisp-type (spec)
  (typecase spec
    ((cons (eql :string) list)
     'string)
    ((cons (eql :repeated) list)
     'vector)
    ((cons (eql :blob) list)
     'nibbles:octet-vector)
    (t
     spec)))


;;; Size method
;;

(defun specs->size (class-name specs)
  `(defmethod size ((object ,class-name))
     (+ ,@(map 'list (rcurry #'spec->size class-name 'object)
	       specs))))

(defun spec->size (spec class-name object)
  (let+ (((name type &rest nil) spec)
	 (accessor-name (symbolicate class-name "-" name)))
    (type-spec->size type `(,accessor-name ,object))))

(defun type-spec->size (type-spec value)
  (etypecase type-spec
    ((cons keyword list)
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

    ((cons (eql unsigned-byte) list)
     (ash (second type-spec) -3))

    (symbol
     `(size ,value))))


;;; Deserializer
;;

(defun specs->deserializer (class-name specs)
  `(defmethod unpack ((source simple-array) (object ,class-name)
		      &optional
		      (start 0))
     (check-type source nibbles:octet-vector)

     (let ((offset start))
       ,@(map 'list (rcurry #'spec->deserializer
			    class-name 'source 'object 'offset)
	      specs)
       (values object (- offset start)))))

(defun spec->deserializer (spec class-name source object offset)
  (let+ (((name type &rest nil) spec)
	 (accessor-name (symbolicate class-name "-" name)))
    `(let+ (((&values value length)
	     ,(type-spec->deserializer type source offset)))
       (declare (type ,(type-spec->lisp-type type) value))
       (setf (,accessor-name ,object) value)
       (incf offset length))))

(defun type-spec->deserializer (type-spec source offset)
  (etypecase type-spec
    ((cons keyword list)
     (destructuring-ecase type-spec
       ((:repeated count-slot sub-type)
	`(iter (repeat (slot-value object ',count-slot)) ;;; TODO(jmoringe): slot access
	       (with offset = ,offset)
	       (let+ (((&values value length)
		       ,(type-spec->deserializer sub-type source offset)))
		 (incf offset length)
		 (collect value  :into result :result-type vector)
		 (summing length :into length*))
	       (finally (return (values result length*)))))

       ((:blob length-slot)
	`(let* ((length (slot-value object ',length-slot))) ;;; TODO(jmoringe): slot access
	   (values (subseq ,source ,offset (+ ,offset length)) length)))

       ((:string length-type)
	`(let+ (((&values length length-length)
		 ,(type-spec->deserializer length-type source offset))
		(data-offset (+ ,offset length-length)))
	   (values (sb-ext:octets-to-string
		    source :start data-offset :end (+ data-offset length))
		   (+ length-length length))))))

    ((cons (eql unsigned-byte) list)
     (ecase (second type-spec)
       (8  `(values (aref ,source ,offset) 1))
       (32 `(values (nibbles:ub32ref/le ,source ,offset) 4))
       (64 `(values (nibbles:ub64ref/le ,source ,offset) 8))))

    (symbol
     `(let ((object (allocate-instance (find-class ',type-spec))))
	(unpack ,source object ,offset)))))


;;; Serializer
;;

(defun specs->serializer (class-name specs)
  `(defmethod pack ((object ,class-name) (source simple-array)
		    &optional
		    (start 0))
     (check-type source nibbles:octet-vector)

     (let ((offset start))
       ,@(map 'list (rcurry #'spec->serializer
			    class-name 'source 'object 'offset)
	      specs)
       (- offset start))))

(defun spec->serializer (spec class-name source object offset)
  (let+ (((name type &rest nil) spec)
	 (accessor-name (symbolicate class-name "-" name)))
    `(let ((value (,accessor-name ,object)))
       (declare (type ,(type-spec->lisp-type type) value))
       (incf offset ,(type-spec->serializer type 'value source offset)))))

(defun type-spec->serializer (type-spec value source offset)
  (etypecase type-spec
    ((cons keyword list)
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
	   (let ((octets (sb-ext:string-to-octets ,value)))
	     (replace ,source octets :start1 offset*)
	     (incf offset* (length octets)))
	   (- offset* ,offset)))))

    ((cons (eql unsigned-byte) list)
     `(progn
	,(ecase (second type-spec)
	   (8  `(setf (aref ,source ,offset) ,value))
	   (32 `(setf (nibbles:ub32ref/le ,source ,offset) ,value))
	   (64 `(setf (nibbles:ub64ref/le ,source ,offset) ,value)))
	,(ash (second type-spec) -3)))

    (symbol
     `(pack ,value ,source ,offset))))
