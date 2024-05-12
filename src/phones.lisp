(in-package :starintel)

(defclass phone (document)
  ((number :accessor phone-number :type string :initarg :number :initform (error "Phone number is required"))
   (carrier :accessor phone-carrier :type string :initarg :carrier :initform "")
   (status :accessor phone-status :type string :initarg :status :initform "")
   (phone-type :accessor phone-type :type string :initarg :phone-type :initform "")))




(defun new-phone (&rest args)
  "Create a Phone document"
  (let ((phone (apply #'make-instance 'phone args)))
    (apply #'hash-id phone args)
    phone))
