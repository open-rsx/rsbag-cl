;;; io.lisp --- Input and output of TIDE log structures.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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
			   condition)))

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
	(let+ (((&values offset block) (scan source :block)))
	  (typecase block
	    (chan    (collect block               :into channels))
	    (indx    (collect block               :into indices))
	    (integer (collect (cons block offset) :into chunks))))
	(finally (return (values channels indices chunks)))))

(defmethod scan ((source stream) (object (eql :block))
		 &optional start)
  (declare (ignore start))

  (let+ ((offset (file-position source))
	 ((&values name length) (unpack source :block-header))
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


;;; Unpacking
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
    (declare (dynamic-extent header))
    (values (sb-ext:octets-to-string header
				     :external-format :ascii
				     :start           0
				     :end             4)
	    (nibbles:ub64ref/le header 4))))

(defmethod unpack ((source stream) (object (eql :block))
		   &optional start)
  (declare (ignore start))

  (let+ (((&values name length) (unpack source :block-header))
	 (name  (find-symbol name #.*package*)) ;;; TODO(jmoringe): bottleneck
	 (class (find-class name)))
    (unpack (read-chunk-of-length (if (eq name 'tide) 10 length) source)  ;;; TODO(jmoringe): hack
	    (allocate-instance class))))

;;; Packing
;;;
;;; Nothing to do since everything is handled by the methods generated
;;; in spec.lisp
