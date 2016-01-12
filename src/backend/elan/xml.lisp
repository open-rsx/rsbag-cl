;;;; xml.lisp --- To- and from-XML conversion for Elan types.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.backend.elan)

;;;

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'boolean))
                       &key &allow-other-keys)
  (cond
    ((string= value "false") nil)
    ((string= value "true")  t)
    (t
     (error "~@<Invalid value for type ~A: ~S~@:>"
            type value))))

(defmethod xloc:->xml ((value symbol)
                       (dest  (eql 'string))
                       (type  (eql 'boolean))
                       &key &allow-other-keys)
  (check-type value boolean)

  (if value "true" "false"))

;;; time-slot/cons

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'time-slot/cons))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@ (id    "TIME_SLOT_ID"))                               ".")
       ((:@ (value "TIME_VALUE")   :type 'timestamp/milliseconds) "."))
      value
    (cons id (milliseconds->nanoseconds value))))

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
          value* (nanoseconds->milliseconds (cdr value))))
  value)

;;; annotation/list

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'annotation/list))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@ (id    "ANNOTATION_ID"))  "ALIGNABLE_ANNOTATION")
       ((:@ (start "TIME_SLOT_REF1")) "ALIGNABLE_ANNOTATION")
       ((:@ (end   "TIME_SLOT_REF2")) "ALIGNABLE_ANNOTATION")
       (associated-value              "ALIGNABLE_ANNOTATION/ANNOTATION_VALUE/text()"))
      value
    (list id start end associated-value)))

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'annotation/list))
                       &key &allow-other-keys)
  (check-type value annotation/list)

  (xloc:with-locations
      (((:@ (id    "ANNOTATION_ID"))  "ALIGNABLE_ANNOTATION")
       ((:@ (start "TIME_SLOT_REF1")) "ALIGNABLE_ANNOTATION")
       ((:@ (end   "TIME_SLOT_REF2")) "ALIGNABLE_ANNOTATION")
       (associated-value              "ALIGNABLE_ANNOTATION/ANNOTATION_VALUE/text()"))
      dest
    (multiple-value-setq (id start end associated-value)
      (values-list value)))
  value)

;;; linguistic-type/list

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'linguistic-type/list))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@ (id "LINGUISTIC_TYPE_ID"))                                 ".")
       ((:@ (graphic-references? "GRAPHIC_REFERENCES") :type 'boolean) ".")
       ((:@ (time-alignable? "TIME_ALIGNABLE")         :type 'boolean) "."))
      value
    (list id graphic-references? time-alignable?)))

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'linguistic-type/list))
                       &key &allow-other-keys)
  (check-type value linguistic-type/list)

  (xloc:with-locations
      (((:@ (id "LINGUISTIC_TYPE_ID"))                                 ".")
       ((:@ (graphic-references? "GRAPHIC_REFERENCES") :type 'boolean) ".")
       ((:@ (time-alignable? "TIME_ALIGNABLE")         :type 'boolean) "."))
      dest
    (multiple-value-setq (id graphic-references? time-alignable?)
      (values-list value)))
  value)

;;; tier/list

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'tier/list))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@   (id                  "TIER_ID"))             ".")
       ((:@   (linguistic-type-ref "LINGUISTIC_TYPE_REF")) ".")
       ((:val annotations :type 'annotation/list)          "ANNOTATION"
        :if-multiple-matches :all))
      value
    (list id linguistic-type-ref annotations)))

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'tier/list))
                       &key &allow-other-keys)
  (check-type value tier/list)

  (xloc:with-locations
      (((:@   (id                  "TIER_ID"))             ".")
       ((:@   (linguistic-type-ref "LINGUISTIC_TYPE_REF")) ".")
       ((:val annotations :type 'annotation/list)          "ANNOTATION"
        :assign-mode :append))
      dest
    (multiple-value-setq (id linguistic-type-ref annotations)
      (values-list value)))
  value)

;;; File

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'version/cons))
                       &key &allow-other-keys)
  (let ((index (or (position #\. value)
                   (error "~@<No \".\" in ~S~@:>" value))))
    (cons (parse-integer value :end index)
          (parse-integer value :start (1+ index)))))

(defmethod xloc:->xml ((value cons)
                       (dest  (eql 'string))
                       (type  (eql 'version/cons))
                       &key &allow-other-keys)
  (check-type value version/cons)

  (format nil "~D.~D" (car value) (cdr value)))

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'file/list))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:@   (format     "FORMAT")    :type 'version/cons)         ".")
       ((:@   (version    "VERSION")   :type 'version/cons)         ".")
       ((:@   (author     "AUTHOR"))                                ".")
       ((:@   (date       "DATE")      :type 'local-time:timestamp) ".")
       ;; ((:@   (time-units "TIME_UNITS"))                            "HEADER")
       ((:@   (urls       "MEDIA_URL"))                             "HEADER/MEDIA_DESCRIPTOR"
        :if-multiple-matches :all)
       ((:val time-slots               :type 'time-slot/cons)       "TIME_ORDER/TIME_SLOT"
        :if-multiple-matches :all)
       ((:val tiers                    :type 'tier/list)            "TIER"
        :if-multiple-matches :all))
      value
    (unless (= +format-version-major+ (car version))
      (cerror "Try to process the file anyway."
              "~@<Cannot process format version ~D.~D (major version ~
               is different from ~D.~D)~@:>"
              (car version) (cdr version)
              +format-version-major+ +format-version-minor+))
    (values (list author date urls time-slots tiers) format version))) ; TODO version should be part of file/list?

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'file/list))
                       &key &allow-other-keys)
  (check-type value file/list)

  (xloc:with-locations
      (((:@   (schema     "xsi:noNamespaceSchemaLocation"))         "."
        :namespaces '(("xsi" . "http://www.w3.org/2001/XMLSchema-instance")))
       ((:@   (format     "FORMAT")    :type 'version/cons)         ".")
       ((:@   (version    "VERSION")   :type 'version/cons)         ".")
       ((:@   (author     "AUTHOR"))                                ".")
       ((:@   (date       "DATE")      :type 'local-time:timestamp) ".")
       ((:@   (time-units "TIME_UNITS"))                            "HEADER")
       ((:@   (urls       "MEDIA_URL"))                             "HEADER/MEDIA_DESCRIPTOR"
        :assign-mode :append)
       ((:val slots                    :type 'time-slot/cons)       "TIME_ORDER/TIME_SLOT"
        :assign-mode :append)
       ((:val tiers                    :type 'tier/list)            "TIER"
        :assign-mode :append)
       ((:val linguistic-types         :type 'linguistic-type/list) "LINGUISTIC_TYPE"
        :assign-mode :append))
      dest
    (let ((version/cons (cons +format-version-major+ +format-version-minor+)))
      (multiple-value-setq (schema format version time-units)
        (values (princ-to-string +schema-url+) version/cons version/cons "milliseconds")))
    (multiple-value-setq (author date urls slots tiers) (values-list value))
    (setf linguistic-types '(("Default" nil t))))
  value)
