;;; package.lisp --- Package definition for view module.
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

(cl:defpackage :rsbag.view
  (:use
   :cl
   :alexandria
   :iterate
   :bind

   :rsbag)

  ;; `multi-sequence-view-mixin'
  (:export
   :view-sequences)

  ;; `serialized' view class and construction function
  (:export
   :serialized

   :make-serialized-view

   :%make-key-function)

  (:documentation
   "This module contains functions and classes that implement
views (as subclasses of `cl:sequence') on data stored in
bags. Currently, the following views are available:
+ serialized :: aggregate the data of multiple channels, serializing
                items using their timestamps."))