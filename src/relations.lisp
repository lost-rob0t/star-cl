(in-package :starintel)

(defclass relation (document)
  ((source :accessor relation-source :type string :initarg :source :initform "")
   (target :accessor relation-target :type string :initarg :target :initform "")
   (predicate :accessor relation-predicate :type string :initarg :predicate :initform "")
   (note :accessor relation-note :type string :initarg :note :initform "")))

(defmethod set-id ((doc relation))
  (ulid-id doc))


(defun new-relation (dataset source target &key note  predicate)
  "Create a New Booker Relation"
  (let ((relation (make-instance 'relation :dataset dataset :source source :target target :note note)))
    (set-meta relation dataset)
    relation))
