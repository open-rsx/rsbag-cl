;;; io.lisp --- Input and output of TIDE log structures.
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


;;; Scan
;;

(defmethod scan :around ((source t) (object t)
			 &optional start)
  (declare (ignore start))
  (handler-bind
      (((and error (not tidelog-condition))
	#'(lambda (condition)
	    (error 'invalid-file-structure
		   :source           source
		   :format-control   "~@<Failed to scan for block ~A: ~A~@:>"
		   :format-arguments (list object (format nil "~A" condition))))))
    (call-next-method)))

(defmethod scan ((source stream) (object (eql :tide))
		 &optional
		 start)
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
		 &optional
		   start)
  (bind ((offset (file-position source))
	 ((:values name length) (unpack source :block-header))
	 (name (intern name #.*package*)))
    (values
     offset
     (case name
       ((chan indx)
	(let ((object (make-instance name))
	      (buffer (binio:make-octet-vector length)))
	  (read-sequence buffer source)
	  (unpack buffer object))) ;;; TODO(jmoringe): make a function; maybe read-octet-vector source length
       (chnk
	(prog1
	    (let ((buffer (binio:make-octet-vector 4)))
	      (read-sequence buffer source)
	      (binio:decode-uint32-le buffer))  ;;; TODO(jmoringe):  make a function
	  (file-position source (+ (file-position source) (- length 4)))))
       (t
	(file-position source (+ (file-position source) length)))))))


;;;
;;

(defmethod unpack :around ((source t) (object t)
			   &optional start)
  (declare (ignore start))
  (handler-bind
      (((and error (not tidelog-condition))
	#'(lambda (condition)
	    (error 'invalid-file-structure
		   :source           source
		   :format-control   "~@<Failed to unpack block ~A: ~A~@:>"
		   :format-arguments (list object (format nil "~A" condition))))))
    (call-next-method)))

(defmethod unpack ((source stream) (object (eql :block-header))
		   &optional
		   start)
  (let ((header (binio:make-octet-vector 12)))
    (unless (= (read-sequence header source) 12)
      (error "Could not read block header"))
    (values (binio:decode-utf8 header 0 4) ;;; TODO(jmoringe): bottleneck
	    (binio:decode-uint64-le header 4))))

(defmethod unpack ((source stream) (object (eql :block))
		   &optional
		   start)
  (bind (((:values name length) (unpack source :block-header))
	 (object (make-instance (intern name #.*package*))) ;;; TODO(jmoringe): bottleneck
	 (buffer (binio:make-octet-vector
		  (if (typep object 'tide) 10 length)))) ;;; TODO(jmoringe): hack
    (read-sequence buffer source)
    (unpack buffer object)))


;;; Packing
;;

(defmethod pack ((object standard-object) (source stream)
		 &optional
		 start)
  (bind ((name   (string (class-name (class-of object))))
	 (length (size object))
	 (buffer (binio:make-octet-vector length))) ;;; TODO(jmoringe): hack
    (pack (cons name length) source)
    (pack object buffer)
    (write-sequence buffer source)))

(defmethod pack ((object cons) (source stream)
		 &optional
		 start)
  (bind (((kind . length) object)
	 (header (binio:make-octet-vector 12)))
    (binio:encode-utf8 kind header 0)
    (binio:encode-uint64-le length header 4)
    (write-sequence header source)))


;;;
;;

(defun uint64->timestamp (value)
 (bind (((:values secs nsecs) (truncate value 1000000000)))
   (local-time:unix-to-timestamp secs :nsec nsecs)))

(defun timestamp->uint64 (value)
  (bind (((:accessors-r/o (secs  local-time:timestamp-to-unix)
			  (nsecs local-time:nsec-of)) value))
    (+ (* (expt 10 9) secs) nsecs)))
