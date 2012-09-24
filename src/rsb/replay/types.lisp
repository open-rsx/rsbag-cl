;;; types.lisp --- types used in the rsb.replay module.
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

(cl:in-package :rsbag.rsb.replay)


;;; Bounds specifications
;;

(deftype range-boundary/timestamp ()
  "Specification of a boundary of a temporal range.
+ NIL
    Use actual beginning or end of the sequence to be bounded.
+ a `non-negative-real'
    Offset in seconds to the start of the sequence.
+ a `negative-real'
    Negative offset in seconds to the end of the sequence.
+ a `local-time:timestamp'
    Absolute point in time within the sequence."
  '(or null
       non-negative-real
       negative-real
       local-time:timestamp))


;;; Timestamp adjustment specifications
;;

(deftype timestamp-adjustment-value/now ()
  "Indicates that the current time (i.e. time of replaying the event)
  should replace the stored timestamp."
  '(eql :now))

(deftype timestamp-adjustment-value/copy ()
  "A value of the form

  (:COPY TIMESTAMP-DESIGNATOR)

indicates that the timestamp designated by TIMESTAMP-DESIGNATOR should
be extracted from the current event and used to replace the stored
timestamp."
  '(cons (eql :copy) (cons timestamp-designator null)))

(deftype timestamp-adjustment-value/delta ()
  `(cons (eql :delta) (cons real null)))

(deftype timestamp-adjustment-value ()
  "Specification of a replacement value for a particular timestamp."
  '(or timestamp-adjustment-value/now
       timestamp-adjustment-value/copy
       timestamp-adjustment-value/delta
       local-time:timestamp))

(deftype timestamp-adjustment-spec ()
  "Replacement rule of the form

  (TIMESTAMP-DESIGNATOR TIMESTAMP-ADJUSTMENT-VALUE)

specifying that the timestamp designated by TIMESTAMP-DESIGNATOR
should be replaced with the timestamp value specified by
TIMESTAMP-ADJUSTMENT-VALUE"
  '(cons timestamp-designator (cons timestamp-adjustment-value null)))
