;;; macros.lisp --- Macros provided by the rsb module.
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

(cl:in-package :rsbag.rsb)

(defmacro with-open-connection ((var connection-form) &body body)
  "Execute BODY with VAR bound to the connection object that is the
result of evaluating CONNECTION-FORM. Ensure that the connection is
properly closed."
  `(with-open-stream (,var ,connection-form)
     ,@body))

(defmacro with-events->bag ((var source dest
			     &rest args
			     &key
			     transports
			     filters
			     timestamp
			     backend
			     bag-class
			     channel-strategy
			     &allow-other-keys)
			    &body body)
  "Execute BODY with VAR bound to a connection that is the result of
applying `events->bag' to SOURCE, DEST and ARGS. Ensure that the
resulting connection is properly closed."
  (declare (ignore transports filters timestamp backend bag-class
		   channel-strategy))
  `(with-open-connection (,var (events->bag ,source, dest ,@args))
     ,@body))

(defmacro with-bag->events ((var source dest
			     &rest args
			     &key
			     backend
			     bag-class
			     replay-strategy
			     start-time
			     start-index
			     end-time
			     end-index
			     channels
			     &allow-other-keys)
			    &body body)
  "Execute BODY with VAR bound to a connection that is the result of
applying `bag->events' to SOURCE, DEST and ARGS. Ensure that the
resulting connection is properly closed."
  (declare (ignore backend bag-class replay-strategy
		   start-time start-index end-time end-index
		   channels))
  `(with-open-connection (,var (bag->events ,source ,dest ,@args))
     ,@body))
