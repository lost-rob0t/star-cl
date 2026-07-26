(in-package #:starintel)

(defclass actor-manifest ()
  ((_id :initarg :id :initform nil :accessor doc-id)
   (_rev :initarg :rev :initform nil :accessor doc-rev)
   (type :initarg :type :initform "actor-manifest" :accessor doc-type)
   (actor :initarg :actor :initform nil :accessor actor-name)
   (consumer-path :initarg :consumers :initform nil :accessor actor-consumers)
   (target-options :initarg :target-options :initform nil :accessor target-options)
   (date-updated :accessor doc-updated :type integer :initarg :date-updated :initform (unix-now))
   (date-added :accessor doc-added :type integer :initarg :date-added :initform (unix-now)))
  (:documentation "Actor manifest describing actor services."))

(defmethod set-id ((document actor-manifest))
  "Set the deterministic actor-manifest ID when missing."
  (when (or (null (doc-id document))
            (and (stringp (doc-id document))
                 (string= (doc-id document) "")))
    (setf (doc-id document)
          (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence
            *default-hash-algo*
            (ironclad:ascii-string-to-byte-array
             (format nil "~a" (actor-name document)))))))
  (doc-id document))

(defmethod timestamp ((document actor-manifest))
  (unless (doc-added document)
    (setf (doc-added document) (unix-now)))
  (unless (doc-updated document)
    (setf (doc-updated document) (unix-now)))
  document)

(defmethod update-timetamp ((document actor-manifest))
  (setf (doc-updated document) (unix-now))
  document)

(defmethod set-type ((document actor-manifest))
  (setf (doc-type document) "actor-manifest"))

(defmethod set-meta ((document actor-manifest) dataset)
  (declare (ignore dataset))
  (set-type document)
  (set-id document)
  (timestamp document)
  document)
