(in-package #:starintel)

(defclass actor-manifest ()
  ((_id :initarg :id :initform nil :accessor doc-id)
   (_rev :initarg :rev :initform nil :accessor doc-rev)
   (type :initarg :type :initform "actor-manifest" :accessor doc-rev)
   (actor :initarg :actor :initform nil :accessor actor-name)
   (conumer-path :initarg :consumers :initform nil :accessor actor-consumers)
   (target-options :initarg :target-options :initform list :accessor target-options)
   (date-updated :accessor doc-updated :type integer :initarg :date-updated :initform (unix-now))
   (date-added :accessor doc-added :type integer :initarg :date-added :initform (unix-now)))
  
  (:documentation "Actor manifest object which advetises acto services."))

(defgeneric ulid-id (actor-manifest)
  (:documentation "Generate a ULID for the actor-manifest."))

(defgeneric timestamp (actor-manifest)
  (:documentation "Set the actor-manifest's 'date-added' and 'date-updated' fields to the current Unix time."))

(defgeneric update-timetamp (actor-manifest)
  (:documentation "Update the actor-manifest's 'date-updated' field to the current Unix time."))

(defgeneric hash-id (actor-manifest &rest data)
  (:documentation "Generate a hash-based ID for the actor-manifest."))

(defgeneric set-id (actor-manifest)
  (:documentation "Set the actor-manifest ID if it's not already set."))

(defgeneric set-type (actor-manifest)
  (:documentation "Set the actor-manifest type based on its class name."))

(defgeneric set-meta (actor-manifest dataset)
  (:documentation "Set the metadata of the actor-manifest, including dataset, timestamp, type, and ID if necessary."))


(defmethod set-id ((doc actor-manifest))
  "Set the ID for a domain actor-manifest"
  (setf (doc-id doc) (ironclad:byte-array-to-hex-string (ironclad:digest-sequence
                                                         *default-hash-algo*
                                                         (ironclad:ascii-string-to-byte-array (format nil "~{~a~}" (actor-name doc)))))))


(defmethod timestamp ((doc actor-manifest))
  "Add the current time in unix to the actor-manifest"
  (when (not (doc-added doc))
    (setf (doc-added doc) (unix-now)))
  (when (not (doc-updated doc))
    (setf (doc-updated doc) (unix-now))))

(defmethod update-timetamp ((doc actor-manifest))
  "Update the doc_updated field to the current unix epoch time."
  (setf (doc-updated doc) (unix-now)))

(defmethod set-type ((doc actor-manifest))
  (let* ((full-type (type-of doc))
         (type-parts (uiop:split-string (symbol-name full-type) :separator ":"))
         (type-name (car (last type-parts))))
    (setf (doc-type doc) (string-downcase type-name))))





(defmethod set-meta ((doc actor-manifest) dataset)
  (setf (doc-dataset doc) dataset)
  (set-type doc)
  (when (or (not (doc-id doc)) (= (length (doc-id doc)) 0))
    (set-id doc))
  doc)
