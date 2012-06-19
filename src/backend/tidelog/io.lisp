;;; io.lisp --- Input and output of TIDE log structures.
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

(cl:in-package :rsbag.backend.tidelog)


;;; Scan
;;

(define-condition-translating-method scan ((source t) (object t)
					   &optional start)
  (((and error (not tidelog-condition)) invalid-tidelog-structure
    :var           condition
    :cause-initarg nil)
   :source           source
   :format-control   "~@<Failed to scan for block ~A~@[ at position ~:D~]: ~A~@:>"
   :format-arguments (list object
			   (when (streamp source)
			     (file-position source))
			   (format nil "~A" condition))))

(defmethod scan :before ((source stream) (object t)
			 &optional start)
  "Seek to position START before starting to scan."
  (when start
    (file-position source start)))

(defmethod scan ((source stream) (object (eql :tide))
		 &optional start)
  (declare (ignore start))

  ;; Consume the TIDE block.
  (unpack source :block)
  ;; Scan through remaining blocks.
  (iter (while (listen source))
	(bind (((:values offset block) (scan source :block)))
	  (typecase block
	    (chan    (collect block               :into channels))
	    (indx    (collect block               :into indices))
	    (integer (collect (cons block offset) :into chunks))))
	(finally (return (values channels indices chunks)))))

(defmethod scan ((source stream) (object (eql :block))
		 &optional start)
  (declare (ignore start))

  (bind ((offset (file-position source))
	 ((:values name length) (unpack source :block-header))
	 (name (intern name #.*package*)))
    (values
     offset
     (case name
       ((chan indx)
	(unpack (read-chunk-of-length length source)
		(allocate-instance (find-class name))))
       (chnk
	(prog1
	    (nibbles:read-ub32/le source)
	  (file-position source (+ (file-position source) (- length 4)))))
       (t
	(file-position source (+ (file-position source) length)))))))


;;;
;;

(define-condition-translating-method unpack ((source t) (object t)
					     &optional start)
  (((and error (not tidelog-condition)) invalid-tidelog-structure
    :var condition)
   :source           source
   :format-control   "~@<Failed to unpack block ~A~@[ at position ~:D~]: ~A~@:>"
   :format-arguments (list object
			   (when (streamp source)
			     (file-position source))
			   (format nil "~A" condition))))

(defmethod unpack :before ((source stream) (object t)
			   &optional start)
  "Seek to position START before unpacking into OBJECT."
  (when start
    (file-position source start)))

(defmethod unpack ((source stream) (object (eql :block-header))
		   &optional start)
  (declare (ignore start))

  (let ((header (read-chunk-of-length 12 source)))
    (values (sb-ext:octets-to-string header
				     :external-format :ascii
				     :start           0
				     :end             4)
	    (nibbles:ub64ref/le header 4))))

(defmethod unpack ((source stream) (object (eql :block))
		   &optional start)
  (declare (ignore start))

  (bind (((:values name length) (unpack source :block-header))
	 (name  (find-symbol name #.*package*)) ;;; TODO(jmoringe): bottleneck
	 (class (find-class name)))
    (unpack (read-chunk-of-length (if (eq name 'tide) 10 length) source)  ;;; TODO(jmoringe): hack
	    (allocate-instance class))))


;;; Packing
;;

(defmethod pack ((object standard-object) (source stream)
		 &optional start)
  (declare (ignore start))

  (bind ((name   (string (class-name (class-of object))))
	 (length (size object))
	 (buffer (nibbles:make-octet-vector length)))
    (pack (cons name length) source)
    (pack object buffer)
    (write-sequence buffer source)))

(defmethod pack ((object cons) (source stream)
		 &optional start)
  (declare (ignore start))

  (bind (((kind . length) object)
	 (header (nibbles:make-octet-vector 12)))
    (declare (type (string 4) kind))
    (replace header (sb-ext:string-to-octets kind :external-format :ascii)) ;;; TODO(jmoringe, 2012-04-13): check length
    (setf (nibbles:ub64ref/le header 4) length)
    (write-sequence header source)))
