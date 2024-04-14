(in-package :starintel)

(defclass geo (document)
  ((lat :accessor geo-lat :type float64 :initarg :lat :initform 0.0)
   (long :accessor geo-long :type float64 :initarg :long :initform 0.0)
   (alt :accessor geo-alt :type float64 :initarg :alt :initform 0.0)))

(defclass address (geo)
  ((city :accessor address-city :type string :initarg :city :initform (error "City is required"))
   (state :accessor address-state :type string :initarg :state :initform (error "State is required"))
   (postal :accessor address-postal :type string :initarg :postal :initform (error "Postal code is required"))
   (country :accessor address-country :type string :initarg :country :initform (error "Country is required"))
   (street :accessor address-street :type string :initarg :street :initform (error "Street address is required"))
   (street2 :accessor address-street2 :type string :initarg :street2)))

(defun new-address (street street2 city postal state country &optional lat long)
  "Create a New Booker Address"
  (let ((address (make-instance 'address :street street :street2 street2 :city city :postal postal :state state :country country :lat lat :long (or long 0.0))))
    (hash-id address (format nil "~a~a~a~a~a~a~a" street street2 city postal state country lat))
    address))
