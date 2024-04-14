(in-package :starintel)

(defclass relation (document)
  ((source :accessor relation-source :type string :initarg :source :initform "")
   (target :accessor relation-target :type string :initarg :target :initform "")
   (note :accessor relation-note :type string :initarg :note :initform "")))


(defun new-relation (source target &optional note dataset)
  "Create a New Booker Relation"
  (let ((relation (make-instance 'relation :source source :target target :note (or note "") :dataset (or dataset ""))))
    (make-uuid relation)
    (time-stamp relation)
    relation))
