(in-package :starintel)

(defclass target ()
  ((_id :accessor target-id :type string :initarg :id :initform "")
   (version :reader doc-version :initform *starintel-doc-version* :type string)
   (actor :accessor target-actor :type string :initarg :actor :initform (error "target requires an actor"))
   (dataset :accessor target-dataset :type string :initarg :dataset :initform (error "target require a dataset"))
   (target :accessor target-target :type string :initarg :target :initform (error "target requires a target to be set."))
   (delay :accessor target-delay :type integer :initform 0 :initarg :delay)
   (recurring :accessor target-recurring-p :type bool :initform nil :initarg :recurring)
   (options :accessor target-options :type list :initarg :options :initform '())))

(defmethod make-uuid ((doc target))
  "Create a UUID for the document"
  (setf (target-id doc) (uuid:make-v4-uuid)))

(defmethod hash-id ((doc target) data &optional (type :md5))
  (setf (target-id doc) (ironclad:byte-array-to-hex-string (ironclad:digest-sequence
                                                            type
                                                            (ironclad:ascii-string-to-byte-array data)))))



(defun new-target (dataset target actor &key (options nil) (delay 0) (recurring nil))
  "Create a New Booker Target"
  (let ((target (make-instance 'target :dataset dataset :target target :actor actor :delay delay :recurring recurring :options (or options (list)))))
    (hash-id target (format nil "~a~a~a~a" dataset (target-target target) actor delay))
    target))

(defun new-target-without-options (dataset target actor)
  "Create a New Booker Target Without Options"
  (new-target dataset target actor))
