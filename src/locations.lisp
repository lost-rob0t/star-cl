(in-package :starintel)

(defclass booker-geo (booker-document)
  ((lat :accessor geo-lat :type float64 :initarg :lat)
   (long :accessor geo-long :type float64 :initarg :long)
   (alt :accessor geo-alt :type float64 :initarg :alt)))

(defclass booker-address (booker-geo)
  ((city :accessor address-city :type string :initarg :city)
   (state :accessor address-state :type string :initarg :state)
   (postal :accessor address-postal :type string :initarg :postal)
   (country :accessor address-country :type string :initarg :country)
   (street :accessor address-street :type string :initarg :street)
   (street2 :accessor address-street2 :type string :initarg :street2)))

(defun new-address (street street2 city postal state country lat &optional long)
  "Create a New Booker Address"
  (let ((address (make-instance 'booker-address :street street :street2 street2 :city city :postal postal :state state :country country :lat lat :long (or long 0.0))))
    (hash-id address (format nil "~a~a~a~a~a~a~a" street street2 city postal state country lat))
    address))
