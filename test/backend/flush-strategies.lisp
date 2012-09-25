;;; flush-strategies.lisp --- Unit tests for flush strategies.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsbag.backend.test)

(defclass mock-buffer ()
  ((length/entries :initarg  :length/entries
		   :accessor length/entries)
   (length/bytes   :initarg  :length/bytes
		   :accessor length/bytes)))

(defmethod buffer-property ((backend t)
			    (buffer  mock-buffer)
			    (name    (eql :length/entries)))
  (length/entries buffer))

(defmethod buffer-property ((backend t)
			    (buffer  mock-buffer)
			    (name    (eql :length/bytes)))
  (length/bytes buffer))

(defmethod buffer-property ((backend t)
			    (buffer  mock-buffer)
			    (name    (eql :time-to-last-write)))
  5)

(deftestsuite flush-strategies-root (backend-root)
  ()
  (:function
   (make-strategy (spec args)
     (apply #'make-flush-strategy spec args)))
  (:documentation
   "Root test suite for tests of flush strategies."))

(defmacro define-basic-flush-strategy-suite ((spec) &body cases)
  (let ((suite-name (format-symbol *package* "~A-ROOT" spec)))
    `(progn
       (deftestsuite ,suite-name (flush-strategies-root)
	 ()
	 (:documentation
	  ,(format nil "Test suite for ~(~A~) flush strategy."
		   spec)))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test constructing ~(~A~) instances."
			  spec))
	 construct

	 (ensure-cases (strategy-args buffer-args expected)
	     (list ,@cases)

	   (if (eq expected :error)
	       (ensure-condition 'error
		 (make-strategy ,spec strategy-args))
	       (ensure-null
		(emptyp
		 (princ-to-string
		  (make-strategy ,spec strategy-args)))))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test constructing ~(~A~) instances."
			  spec))
	 smoke

	 (ensure-cases (strategy-args buffer-args expected)
	     (remove :error (list ,@cases) :key #'third)

	   (let ((strategy (make-strategy ,spec strategy-args))
		 (buffer   (apply #'make-instance 'mock-buffer buffer-args)))
	     (ensure-same (flush? strategy t buffer) expected)))))))

(define-basic-flush-strategy-suite (:property-limit)
  ;; Missing required initargs.
  '(() ()                                      :error)
  ;; Missing :property initarg.
  '((:property :foo) ()                        :error)
  ;; Missing :limit initarg.
  '((:limit 10) ()                             :error)
  ;; Specified property does not exist.
  '((:property :foo :limit 10) ()              :error)

  ;; These are OK.
  '((:property :length/entries :limit 10)
    (:length/entries 10 :length/bytes 999999)  nil)
  '((:property :length/entries :limit 10)
    (:length/entries 11 :length/bytes 1000001) t)
  '((:property :length/bytes :limit 1000000)
    (:length/entries 10 :length/bytes 999999)  nil)
  '((:property :length/bytes :limit 1000000)
    (:length/entries 11 :length/bytes 1000001) t))

(define-basic-flush-strategy-suite (:or)
  ;; No such subordinate strategy.
  '(((:no-such-strategy)) ()                   :error)
  ;; Invalid initargs for subordinate strategy.
  '(((:property-limit :property :foo :limit 10))
    ()                                         :error)

  ;; These are OK
  '(() ()                                      nil)
  '(((:property-limit :property :length/entries     :limit 10))
    (:length/entries 9 :length/bytes 1000)     nil)
  '(((:property-limit :property :length/entries     :limit 10)
     (:property-limit :property :time-to-last-write :limit 2))
    (:length/entries 10 :length/bytes 1000)    t)
  '(((:property-limit :property :length/entries     :limit 10)
     (:property-limit :property :time-to-last-write :limit 2))
    (:length/entries 11 :length/bytes 1000)    t))

(define-basic-flush-strategy-suite (:and)
  ;; No such subordinate strategy.
  '(((:no-such-strategy)) ()                   :error)
  ;; Invalid initargs for subordinate strategy.
  '(((:property-limit :property :foo :limit 10))
    ()                                         :error)

  ;; These are OK
  '(() ()                                      t)
  '(((:property-limit :property :length/entries     :limit 10))
    (:length/entries 9 :length/bytes 1000)     nil)
  '(((:property-limit :property :length/entries     :limit 10)
     (:property-limit :property :time-to-last-write :limit 2))
    (:length/entries 10 :length/bytes 1000)    nil)
  '(((:property-limit :property :length/entries     :limit 10)
     (:property-limit :property :time-to-last-write :limit 2))
    (:length/entries 11 :length/bytes 1000)    t))

(define-basic-flush-strategy-suite (:not)
  ;; No such subordinate strategy.
  '(((:no-such-strategy)) ()                   :error)
  ;; Invalid initargs for subordinate strategy.
  '(((:property-limit :property :foo :limit 10))
    ()                                         :error)

  ;; These are OK
  '(() ()                                      t)
  '(((:property-limit :property :length/entries     :limit 10))
    (:length/entries 9 :length/bytes 1000)     t)
  '(((:property-limit :property :length/entries     :limit 10)
     (:property-limit :property :time-to-last-write :limit 2))
    (:length/entries 10 :length/bytes 1000)    nil)
  '(((:property-limit :property :length/entries     :limit 10)
     (:property-limit :property :time-to-last-write :limit 2))
    (:length/entries 11 :length/bytes 1000)    nil))
