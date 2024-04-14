(in-package :starintel)

(defclass phone (document)
  ((number :accessor phone-number :type string :initarg :number :initform (error "Phone number is required"))
   (carrier :accessor phone-carrier :type string :initarg :carrier :initform "")
   (status :accessor phone-status :type string :initarg :status :initform "")
   (phone-type :accessor phone-type :type string :initarg :phone-type :initform "")))


(defun new-phone (number carrier status &optional phone-type)
  "Create a New Booker Phone"
  (let ((phone (make-instance 'phone :number number :carrier carrier :status status :phone-type (or phone-type "") :dtype "phone")))
    (hash-id phone number)
    phone))
