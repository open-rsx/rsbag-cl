;;; replay-restart-mixin.lisp --- replay-restart-mixin mixin class.
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

(defclass replay-restart-mixin ()
  ()
  (:documentation
   "This mixin class add the establishing of continue and log restarts
around the actual work of the `replay' method."))

(defmethod replay :around ((connection replay-bag-connection)
			   (strategy   replay-restart-mixin)
			   &key &allow-other-keys)
  (handler-bind
      ((error #'(lambda (condition)
		  (restart-case
		      (error 'event-retrieval-failed
			     :connection connection
			     :strategy   strategy
			     :cause      condition)
		    (continue (&optional condition)
		      :report (lambda (stream)
				(format stream "~@<Ignore the ~
failed event and continue with the next event.~@:>"))
                      (declare (ignore condition))
		      (use-value :skip))
		    (log (&optional condition)
		      :report (lambda (stream)
				(format stream "~@<Log an error ~
message and continue with the next event.~@:>"))
		      (log1 :error "Failed to retrieve an event for replay: ~A"
			    condition)
		      (use-value :skip))))))
    (call-next-method)))
