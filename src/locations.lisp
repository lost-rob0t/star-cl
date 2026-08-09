(in-package :starintel)

(defclass geo (document)
  ((lat :accessor geo-lat :type float64 :initarg :lat :initform 0.0)
   (long :accessor geo-long :type float64 :initarg :long :initform 0.0)
   (alt :accessor geo-alt :type float64 :initarg :alt :initform 0.0)))

(defclass address (geo)
  ((city :accessor address-city :type string :initarg :city :initform "")
   (state :accessor address-state :type string :initarg :state :initform "")
   (postal :accessor address-postal :type string :initarg :postal :initform "")
   (country :accessor address-country :type string :initarg :country :initform "")
   (street :accessor address-street :type string :initarg :street :initform "")
   (street2 :accessor address-street2 :type string :initarg :street2 :initform "")))

(defmethod set-id ((document geo))
  (when (document-id-missing-p document)
    (hash-id document
             (geo-lat document)
             (geo-long document)
             (geo-alt document)))
  (doc-id document))

(defmethod set-id ((document address))
  (when (document-id-missing-p document)
    (hash-id document
             (geo-lat document)
             (geo-long document)
             (geo-alt document)
             (address-city document)
             (address-state document)
             (address-postal document)
             (address-country document)
             (address-street document)
             (address-street2 document)))
  (doc-id document))

(defun new-geo (dataset &rest args)
  "Create a New Geo"
  (let ((geo (apply #'make-instance 'geo args)))
    (set-meta geo dataset)
    geo))

(defun new-address (dataset &rest args)
  "Create a New Booker Address"
  (let ((address (apply #'make-instance 'address args)))
    (set-meta address dataset)
    address))
