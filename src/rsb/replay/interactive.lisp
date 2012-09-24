;;; interactive.lisp --- A strategy for interactive replay control.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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
