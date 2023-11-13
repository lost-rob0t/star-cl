(in-package :starintel)
(defclass booker-relation (booker-document)
  ((source :accessor relation-source :type string :initarg :source)
   (target :accessor relation-target :type string :initarg :target)
   (note :accessor relation-note :type string :initarg :note)))

(defun new-relation (source target &optional note dataset)
  "Create a New Booker Relation"
  (let ((relation (make-instance 'booker-relation :source source :target target :note (or note "") :dataset (or dataset ""))))
    (make-uuid relation)
    (time-stamp relation)
    relation))
