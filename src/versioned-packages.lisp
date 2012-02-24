;;; versioned-packages.lisp --- Management of multiple versions of packages.
;;
;; Copyright (C) 2012 Jan Moringen
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

(defun make-versioned-name (name version)
  "Return a keyword consisting of NAME and VERSION."
  (format-symbol :keyword "~A-~A" name version))

(defmacro with-renamed-package ((old-name new-name
					  &optional
					  (temp-name (format nil "~A-TEMP" old-name)))
				&body body)
  "Execute BODY with the package designated by OLD-NAME moved out of
the way, then rename the package designated by OLD-NAME to
NEW-NAME. This allow BODY to seemingly load stuff into OLD-NAME which
will end up in NEW-NAME without disturbing the contents of OLD-NAME."
  (with-unique-names (package)
    `(let ((,package (find-package ,old-name)))
       (%maybe-delete-package ,new-name)
       (%maybe-delete-package ,temp-name)
       (when ,package
	 (rename-package ,package ,temp-name))
       (unwind-protect
	    (prog1
		,@body
	      (rename-package (find-package ,old-name) ,new-name))
	 (%maybe-delete-package ,old-name)
	 (when ,package
	   (rename-package ,package ,old-name))))))

(defmacro with-renamed-packages ((&rest renames) &body body)
  "Execute BODY with multiple renamings in the sense of
`with-renamed-package'. RENAMES is a list of items of the form

  (OLD-NAME NEW-NAME)

."
  (labels ((wrap (renames body)
	     (if renames
		 `(with-renamed-package ,(first renames)
		    ,(wrap (rest renames) body))
		 `(progn ,@body))))
    (wrap renames body)))

(defmacro with-versioned-packages ((version &rest packages) &body body)
  "Execute body with certain package renamings in the sense of
`with-renamed-packages'. PACKAGES are renamed to names suffixed with
VERSION."
  (check-type version string)

  (flet ((without-and-with-version (name)
	   (list name (make-versioned-name name version))))
    `(with-renamed-packages ,(map 'list #'without-and-with-version
				  packages)
       ,@body)))


;;; Utility functions
;;

(defun %maybe-delete-package (designator)
  "If DESIGNATOR designates a package, delete it."
  (when (find-package designator)
    (delete-package designator)))
