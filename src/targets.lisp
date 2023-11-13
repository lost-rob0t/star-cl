(in-package :starintel)
(defclass booker-target (booker-root-obj)
  ((id :accessor target-id :type string :initarg :id)
   (actor :accessor target-actor :type string :initarg :actor)
   (dataset :accessor target-dataset :type string :initarg :dataset)
   (target :accessor target-target :type string :initarg :target)
   (options :accessor target-options :type list :initarg :options)))

(defun new-target (dataset target actor &optional options)
  "Create a New Booker Target"
  (let ((target (make-instance 'booker-target :id (generate-uuid) :dataset dataset :target target :actor actor :options (or options '()))))
    (hash-id target (format nil "~a~a~a" dataset target actor))
    target))

(defun new-target-without-options (dataset target actor)
  "Create a New Booker Target Without Options"
  (new-target dataset target actor))
