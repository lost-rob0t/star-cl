(in-package :starintel)

(defclass phone (document)
  ((number :accessor phone-number :type string :initarg :number :initform "")
   (carrier :accessor phone-carrier :type string :initarg :carrier :initform "")
   (status :accessor phone-status :type string :initarg :status :initform "")
   (phone-type :accessor phone-type :type string :initarg :phone-type :initform "")))

(defmethod set-id ((doc phone))
  (hash-id doc
           (phone-number doc)))


(defun new-phone (dataset &rest args)
  "Create a Phone document"
  (let ((phone (apply #'make-instance 'phone args)))
    (set-meta phone dataset)
    phone))
