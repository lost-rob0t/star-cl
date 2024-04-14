(in-package :starintel)
(defclass target (root-obj)
  ((id :accessor target-id :type string :initarg :id)
   (actor :accessor target-actor :type string :initarg :actor)
   (dataset :accessor target-dataset :type string :initarg :dataset)
   (target :accessor target-target :type string :initarg :target)
   (options :accessor target-options :type list :initarg :options)))

(defun new-target (dataset target actor &optional options)
  "Create a New Booker Target"
  (let ((target (make-instance 'target :id (generate-uuid) :dataset dataset :target target :actor actor :options (or options '()))))
    (hash-id target (format nil "~a~a~a" dataset target actor))
    target))

(defun new-target-without-options (dataset target actor)
  "Create a New Booker Target Without Options"
  (new-target dataset target actor))
