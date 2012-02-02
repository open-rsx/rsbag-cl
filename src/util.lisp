;;; util.lisp --- Utilities used in the cl-rsbag system.
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

(cl:in-package :rsbag)

(defmacro define-plist-data-mixin (name
				   &key
				   (slot-name name))
  "Define a class `plist-NAME-mixin' which manages a plist in a
slot. Define the following accessors along with the class:
+ `NAME-count' :: Return number of items.
+ `NAME-keys' :: Return item keys.
+ `NAME-values' :: Return item values.
+ `NAME-plist' :: Return items as plist.
+ `NAME-alist' :: Return items as alist."
  (bind ((class-name (symbolicate "PLIST-" name "-MIXIN"))
	 (initarg    (make-keyword slot-name))
	 ((count-name keys-name values-name plist-name alist-name)
	  (map 'list (curry #'symbolicate name)
	       '("-COUNT" "-KEYS" "-VALUES" "-PLIST" "-ALIST"))))
    `(progn
       (defclass ,class-name ()
	 ((,slot-name :initarg  ,initarg
		      :type     list
		      :initform nil
		      :documentation

		      ,(format nil "Stores the ~(~A~) items associated ~
to the instance."
			       name)))
	 (:documentation
	  "This mixin adds storage for a plist of items and associated
accessors. See `define-plist-data-mixin' for a description."))

       (defgeneric ,count-name (object)
	 (:method ((object ,class-name))
	   (ash (length (slot-value object ',slot-name)) -1))
	 (:documentation
	  ,(format nil "Return the number of ~(~A~) items stored in OBJECT."
		   name)))

       (defgeneric ,keys-name (object)
	 (:method ((object ,class-name))
	   (iter (for (key) on (slot-value object ',slot-name)
		      :by #'cddr)
		 (collect key)))
	 (:documentation

	  ,(format nil "Return a list of the keys of ~(~A~) items ~
stored in OBJECT."
		   name)))

       (defgeneric ,values-name (object)
	 (:method ((object ,class-name))
	   (iter (for (key value) on (slot-value object ',slot-name)
		      :by #'cddr)
		 (collect value)))
	 (:documentation
	  ,(format nil "Return a list of the values of ~(~A~) items ~
stored in OBJECT."
		   name)))

       (defgeneric ,plist-name (object)
	 (:method ((object ,class-name))
	   (slot-value object ',slot-name))
	 (:documentation
	  ,(format nil "Return a plist of the ~(~A~) items stored in ~
OBJECT."
		   name)))

       (defgeneric ,alist-name (object)
	 (:method ((object ,class-name))
	   (plist-alist (slot-value object ',slot-name)))
	 (:documentation
	  ,(format nil "Return an alist of the ~(~A~) items stored ~
in OBJECT."
		   name)))

       (defgeneric ,name (object key)
	 (:method ((object ,class-name) (key t))
	   (getf (slot-value object ',slot-name) key))
	 (:documentation
	  ,(format nil "Return the ~(~A~) item of OBJECT identified ~
by KEY."
		   name)))

       (defgeneric (setf ,name) (new-value object key)
	 (:method ((new-value t) (object ,class-name) (key t))
	   (setf (getf (slot-value object ',slot-name) key) new-value))
	 (:documentation

	  ,(format nil "Associate NEW-VALUE to OBJECT as the ~(~A~)
item identified by KEY."
		   name))))))

(define-plist-data-mixin meta-data)
