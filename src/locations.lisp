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
   (street2 :accessor address-street2 :type string :initarg :street2)))

(defmethod set-id ((doc geo))
  (hash-id doc
           (geo-lat doc)
           (geo-long doc)
           (geo-alt doc)))

(defmethod set-id ((doc address))
  (hash-id doc
           (address-lat doc)
           (address-long doc)
           (address-alt doc)
           (address-city doc)
           (address-state doc)
           (address-postal doc)
           (address-country doc)
           (address-street doc)
           (address-street2 doc)))

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
