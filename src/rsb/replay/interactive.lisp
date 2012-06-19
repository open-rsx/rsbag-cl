;;; interactive.lisp --- A strategy for interactive replay control.
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

(defmethod find-replay-strategy-class ((spec (eql :interactive)))
  (find-class 'interactive))

(defclass interactive (error-policy-mixin
		       external-driver-mixin)
  ((stream           :initarg  :stream
		     :type     stream
		     :accessor strategy-stream
		     :initform *query-io*
		     :documentation
		     "Stores the bi-directional stream that is used
for user interaction.")
   (previous-command :initarg  :previous-command
		     :type     (or null function)
		     :accessor strategy-previous-command
		     :initform nil
		     :documentation
		     "Stores the previously invoked command or nil."))
  (:documentation
   "This strategy allows controlling the replay process interactively
by means of textual commands."))

(defmethod next-command ((strategy interactive))
  (let+ (((&accessors-r/o (commands strategy-commands)
			  (stream   strategy-stream)) strategy)
	 ((&accessors (previous strategy-previous-command)) strategy))

    ;; Read commands an existing one is specified.
    (iter (for (name . args) next (%read-command stream))
	  (for command       next (if name
				      (find-command strategy name
						    :test   #'starts-with-subseq
						    :error? nil)
				      previous))
	  (if command
	      (progn
		(setf previous command)
		(return #'(lambda ()
			    (when-let ((result (multiple-value-list
						(apply command args))))
			      (format stream "~A~%" (first result))))))
	      (format stream "~@<~:[Cannot repeat command without ~
previous command~;~:*No such command: ~S~]. Available commands: ~
~{~(~A~)~^, ~}.~@:>~%"
		      name (map 'list #'car commands))))))

(defmethod execute-command ((strategy interactive)
			    (command  function))
  (let+ (((&accessors-r/o (stream strategy-stream)) strategy))
    (handler-case
	(funcall command)
      (error (condition)
	(format stream "~&~@<Error executing command ~A: ~A~@:>~%"
		command condition)))))

(defmethod replay ((connection replay-bag-connection)
		   (strategy   interactive)
		   &key &allow-other-keys)
  (let+ (((&accessors-r/o (stream strategy-stream)) strategy))
    (format stream "~&~@<OHAI, type command; unambiguous prefix ~
suffices. empty command repeats previous one.~@:>~%")
    (call-next-method)
    (format stream "~&~@<KTHXBYE~@:>~%")))


;;; Utility functions
;;

(defun %read-command (stream)
  (let+ ((line (progn
		 (format stream "~&> ")
		 (force-output stream)
		 (read-line stream nil "quit")))
	 ((&optional name &rest args)
	  (split-sequence #\Space line
			  :remove-empty-subseqs t)))
    (when name
      (cons (string-upcase name)
	    (map 'list #'read-from-string args)))))
