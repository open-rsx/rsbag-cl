;;; view-creation-mixin.lisp --- Mixin class for creating multi-channel views.
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

(cl:in-package :rsbag.rsb.replay)

(defclass view-creation-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into replay strategy classes
that have to construct a view sequence for multiple channels. The
generic function `make-view' can be used to customize this
behavior. The method for `view-creation-mixin' creates a
serialized view of events across channels."))

(defmethod make-view ((connection replay-bag-connection)
		      (strategy   view-creation-mixin)
		      &key
		      (selector (rcurry #'inject-informer connection)))
  "Default behavior is serializing events across channels."
  (make-serialized-view
   (mappend #'connection-channels (connection-channels connection))
   :selector selector))
