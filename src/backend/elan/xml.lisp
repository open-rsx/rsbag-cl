;;; xml.lisp --- To- and from-XML conversion for Elan types.
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

(cl:in-package :rsbag.backend.elan)


;;; time-slot/cons
;;

(defmethod xloc:xml-> ((value stp:element)
		       (type  (eql 'time-slot/cons))
		       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@ (id    "TIME_SLOT_ID"))                               ".")
       ((:@ (value "TIME_VALUE")   :type 'timestamp/milliseconds) "."))
      value
    (cons id value)))

(defmethod xloc:->xml ((value cons)
		       (dest  stp:element)
		       (type  (eql 'time-slot/cons))
		       &key &allow-other-keys)
  (check-type value time-slot/cons)

  (xloc:with-locations
      (((:@ (id     "TIME_SLOT_ID"))                               ".")
       ((:@ (value* "TIME_VALUE")   :type 'timestamp/milliseconds) "."))
      dest
    (setf id     (car value)
	  value* (cdr value)))
  value)


;;; annotation/list
;;

(defmethod xloc:xml-> ((value stp:element)
		       (type  (eql 'annotation/list))
		       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@ (start "TIME_SLOT_REF1")) ".")
       ((:@ (end   "TIME_SLOT_REF2")) ".")
       (associated-value              "ANNOTATION_VALUE/text()"))
      value
    (list start end associated-value)))

(defmethod xloc:->xml ((value list)
		       (dest  stp:element)
		       (type  (eql 'annotation/list))
		       &key &allow-other-keys)
  (check-type value annotation/list)

  (xloc:with-locations
      (((:@ (start "TIME_SLOT_REF1")) ".")
       ((:@ (end   "TIME_SLOT_REF2")) ".")
       (associated-value              "ANNOTATION_VALUE/text()"))
      dest
    (multiple-value-setq (start end associated-value)
      (values-list value)))
  value)


;;; tier/list
;;

(defmethod xloc:xml-> ((value stp:element)
		       (type  (eql 'tier/list))
		       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@   (id "TIER_ID")) ".")
       ((:val annotations :type 'annotation/list)
	"ANNOTATION/ALIGNABLE_ANNOTATION"
	:if-multiple-matches :all))
      value
    (list id annotations)))

(defmethod xloc:->xml ((value list)
		       (dest  stp:element)
		       (type  (eql 'tier/list))
		       &key &allow-other-keys)
  (check-type value tier/list)

  (xloc:with-locations
      (((:@   (id "TIER_ID")) ".")
       ((:val annotations :type 'annotation/list)
	"ANNOTATION/ALIGNABLE_ANNOTATION"
	:assign-mode :append))
      dest

    (multiple-value-setq (id annotations) (values-list value)))
  value)


;;; File
;;

(defmethod xloc:xml-> ((value stp:element)
		       (type  (eql 'file/list))
		       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@   (date "DATE") :type 'local-time:timestamp) ".")
       ((:@   (urls "MEDIA_URL"))                        "HEADER/MEDIA_DESCRIPTOR"
	:if-multiple-matches :all)
       ((:val time-slots :type 'time-slot/cons)          "TIME_ORDER/TIME_SLOT"
	:if-multiple-matches :all)
       ((:val tiers      :type 'tier/list)               "TIER"
	:if-multiple-matches :all))
      value
    (list date urls time-slots tiers)))

(defmethod xloc:->xml ((value list)
		       (dest  stp:element)
		       (type  (eql 'file/list))
		       &key &allow-other-keys)
  (check-type value file/list)

  (xloc:with-locations
      (((:@   (date "DATE")      :type 'local-time:timestamp)
	".")
       ((:@   (urls "MEDIA_URL"))
	"HEADER/MEDIA_DESCRIPTOR"
	:assign-mode :append)
       ((:val slots              :type 'time-slot/cons)
	"TIME_ORDER/TIME_SLOT"
	:assign-mode :append)
       ((:val tiers              :type 'tier/list)
	"TIER"
	:assign-mode :append))
      dest
    (multiple-value-setq (date urls slots tiers) (values-list value)))
  value)
