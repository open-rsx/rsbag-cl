;;; bag.lisp --- The bag class represent data channels stored in a file.
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

(in-package :rsbag)

(defclass bag ()
  ((channels  :type     hash-table
	      :reader   %bag-channels
	      :initform (make-hash-table :test #'equal)
	      :documentation
	      "Stores a mapping of channel names to `channel'
instances.")
   (direction :initarg  :direction
	      :type     direction
	      :reader   bag-direction
	      :documentation
	      "Stores the direction of the bag.")
   (backend   :initarg  :backend
	      :reader   %bag-backend
	      :documentation
	      "Stores an object which is responsible for accessing the
stream associated to this bag."))
  (:default-initargs
   :direction (required-argument :direction)
   :backend   (required-argument :backend))
  (:documentation
   "Instances of this class represent a TIDE file. A TIDE file
consists of named channels which can be retrieved using the
`bag-channels' and `bag-channel' methods and modified using
the `(setf bag-channel)' method. "))

(defmethod shared-initialize :after ((instance   bag)
                                     (slot-names t)
                                     &key)
  (bind (((:accessors-r/o (channels %bag-channels)
			  (backend  %bag-backend)) instance))
    (iter (for (id name meta-data) in (get-channels backend))
	  (setf (gethash name channels)
		(%make-channel instance name meta-data id)))))

(defmethod close ((bag bag)
		  &key &allow-other-keys)
  (close (%bag-backend bag)))

(defmethod bag-channels ((bag bag))
  (hash-table-values (%bag-channels bag)))

(defmethod bag-channel ((bag bag) (name string)
			&key
			(if-does-not-exist :error))
  (or (gethash name (%bag-channels bag))
      (ecase if-does-not-exist
	(:error (error "No such channel ~S" name)) ;;; TODO(jmoringe):
	((nil)  nil))))

(defmethod (setf bag-channel) :before ((new-value t)
				       (bag       t)
				       (name      t)
				       &key &allow-other-keys)
  (when (eq (bag-direction bag) :input)
    (error "Bag ~A has not been opened for output." bag))) ;;; TODO(jmoringe):

(defmethod (setf bag-channel) ((new-value list)
			       (bag       bag)
			       (name      string)
			       &key
			       (if-exists :error))
  (when (gethash name (%bag-channels bag))
    (case if-exists
      (:error     (error "Channel ~S already exists" name)) ;;; TODO(jmoringe): condition
      (:supersede (error "Superseding not implemented")))) ;;; TODO(jmoringe): implement

  (bind (((:accessors-r/o (channels %bag-channels)
			  (backend  %bag-backend)) bag)
	 (channel (%make-channel bag name new-value)))
    (put-channel backend (%channel-id channel) name new-value)
    (setf (gethash name channels) channel)
    channel))

(defmethod print-object ((object bag) stream)
  (bind (((:accessors-r/o (direction bag-direction)
			  (channels  %bag-channels)) object))
   (print-unreadable-object (object stream :type t :identity t)
     (format stream "~:[-~;r~]~:[-~;w~] (~D)"
	     (member direction '(:input :io))
	     (member direction '(:output :io))
	     (hash-table-count channels)))))


;;;
;;

(defmethod %channel-class ((bag bag))
  (find-class 'channel))

(defmethod %make-channel ((bag       bag)
			  (name      string)
			  (meta-data list)
			  &optional
			  id)
  (bind (((:accessors-r/o (backend       %bag-backend)
			  (channel-class %channel-class)) bag))
    (make-instance channel-class
		   :bag       bag
		   :name      name
		   :id        (or id (make-channel-id backend name))
		   ;; :meta-data meta-data
		   :backend   backend)))
