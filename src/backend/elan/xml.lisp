;;;; xml.lisp --- To- and from-XML conversion for Elan types.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.elan)

;;; time-slot/cons

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

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'file/list))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@   (author "AUTHOR"))                              ".")
       ((:@   (date   "DATE")    :type 'local-time:timestamp) ".")
       ((:@   (urls   "MEDIA_URL"))                           "HEADER/MEDIA_DESCRIPTOR"
        :if-multiple-matches :all)
       ((:val time-slots         :type 'time-slot/cons)       "TIME_ORDER/TIME_SLOT"
        :if-multiple-matches :all)
       ((:val tiers              :type 'tier/list)            "TIER"
        :if-multiple-matches :all))
      value
    (list author date urls time-slots tiers)))

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'file/list))
                       &key &allow-other-keys)
  (check-type value file/list)

  (xloc:with-locations
      (((:@   (author "AUTHOR"))                              ".")
       ((:@   (date   "DATE")    :type 'local-time:timestamp) ".")
       ((:@   (urls   "MEDIA_URL"))                           "HEADER/MEDIA_DESCRIPTOR"
        :assign-mode :append)
       ((:val slots              :type 'time-slot/cons)       "TIME_ORDER/TIME_SLOT"
        :assign-mode :append)
       ((:val tiers              :type 'tier/list)            "TIER"
        :assign-mode :append))
      dest
    (multiple-value-setq (author date urls slots tiers) (values-list value)))
  value)
