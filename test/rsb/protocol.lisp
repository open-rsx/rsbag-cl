;;; protocol.lisp --- Unit tests for the protocol of the rsb module.
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

(cl:in-package :rsbag.rsb.test)

(deftestsuite events->bag-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `events->bag' function."))

(addtest (events->bag-root
          :documentation
	  "Smoke test for the `events->bag' function.")
  smoke

  (ensure-cases (args events expected)
      `(;; Invalid channel strategy => error
	((:channel-strategy :no-such-strategy)
	 nil
	 no-such-channel-strategy-class)

	;; These are valid.
	((:channel-strategy :scope-and-type)
	 (1 2 3)
	 (,(octet-vector 1 0 0 0)
	  ,(octet-vector 2 0 0 0)
	  ,(octet-vector 3 0 0 0))))

    (let+ (((&flet do-it ()
	      (with-mock-bag (bag :direction :output) ()
		(with-open-connection
		    (connection (apply #'events->bag '("inprocess:") bag args))
		  (rsb:with-informer (informer "inprocess:" t)
		    (mapc (lambda (datum)
			    (let ((buffer (make-octet-vector 4)))
			      (setf (ub32ref/le buffer 0) datum)
			      (rsb:send informer buffer
					:rsb.transport.wire-schema :uint32)))
			  events))
		  (map 'list #'rsb:event-data (first (bag-channels bag))))))))
     (case expected
       (no-such-channel-strategy-class
	(ensure-condition 'no-such-channel-strategy-class (do-it)))
       (t
	(ensure-same (do-it) expected :test #'equalp))))))

(deftestsuite bag->events-root (rsb-root)
  ()
  (:documentation
   "Test suite for the `bag->events' function."))

(addtest (bag->events-root
          :documentation
	  "Smoke test for the `bag->events' function.")
  smoke

  (ensure-cases (args &optional expected)
      '(;; Invalid channel strategy => error
	((:replay-strategy :no-such-strategy)
	 no-such-replay-strategy-class)

	;; Cannot supply and arguments which would have applied to
	;; opening the bag => error
	((:backend   :does-not-matter) incompatible-arguments)
	((:transform :does-not-matter) incompatible-arguments)
	((:bag-class :does-not-matter) incompatible-arguments)

	;; These are valid.
	((:replay-strategy :as-fast-as-possible)))

    (let+ (((&flet do-it ()
	      (collecting-events (record)
		(with-open-connection
		    (connection
		     (apply #'bag->events (simple-bag) #'record args))
		  (replay connection (connection-strategy connection)))
		(record)))))
      (case expected
	(no-such-replay-strategy-class
	 (ensure-condition 'no-such-replay-strategy-class (do-it)))
	(incompatible-arguments
	 (ensure-condition 'incompatible-arguments (do-it)))
	(t
	 (ensure-same
	  (length (do-it))
	  (reduce #'+ (bag-channels (simple-bag)) :key #'length)))))))

(addtest (bag->events-root
          :documentation
	  "Ensure that events replayed via RSB by `bag->events' get
the configured prefix scope.")
  prefix-scope

  (ensure-cases (prefix)
      '("/" "/prefix")

    (rsb:with-reader (reader "inprocess:")
      ;; Send the events stored in the mock bag.
      (with-open-connection
	  (connection (bag->events
                       (simple-bag) (format nil "inprocess:~A" prefix)
                       :replay-strategy :as-fast-as-possible))
	(replay connection (connection-strategy connection)))
      ;; Receive the events.
      (iter (repeat (reduce #'+ (bag-channels (simple-bag)) :key #'length))
	    (let ((scope (rsb:event-scope (rsb:receive reader))))
	      (ensure (not (rsb:scope= scope prefix)))
	      (ensure (rsb:sub-scope? scope prefix)))))))
