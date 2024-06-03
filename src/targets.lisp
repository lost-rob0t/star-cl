(in-package :starintel)

(defclass target (document)
  ((actor :accessor target-actor :type string :initarg :actor :initform "")
   (target :accessor target-target :type string :initarg :target :initform "")
   (delay :accessor target-delay :type integer :initform 0 :initarg :delay)
   (recurring :accessor target-recurring-p :type bool :initform nil :initarg :recurring)
   (options :accessor target-options :type list :initarg :options :initform nil)))






(defmethod set-id ((target target))
  (hash-id target (format nil "~a~a~a"
                          (doc-dataset target)
                          (target-target target)
                          (target-actor target))))


(defun new-target (dataset target actor &key (options nil) (delay 0) (recurring nil))
  "Create a New Booker Target"
  (let ((target (make-instance 'target :dataset dataset :target target :actor actor :delay delay :recurring recurring :options (or options (list)))))
    (set-meta target dataset)
    target))

(defun new-target-without-options (dataset target actor)
  "Create a New Booker Target Without Options"
  (new-target dataset target actor))
