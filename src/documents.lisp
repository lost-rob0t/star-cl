(in-package :starintel)

(defparameter +starintel-doc-version+ "0.9.0")
(defparameter +legacy-starintel-doc-version+ "0.8.0")
(defparameter *default-hash-algo* :md5)

(defun unix-now ()
  (- (local-time:timestamp-to-universal (local-time:now))
     (encode-universal-time 0 0 0 1 1 1970 0)))

(defclass document ()
  ((_id :accessor doc-id
        :type string
        :initarg :id
        :initform (cms-ulid:ulid))
   (_rev :accessor doc-rev
         :type (or null string)
         :initarg :rev
         :initform nil)
   (dataset :accessor doc-dataset
            :type string
            :initarg :dataset
            :initform "")
   (dtype :accessor doc-type
          :initarg :dtype
          :initform nil)
   (sources :accessor doc-sources
            :type list
            :initarg :sources
            :initform nil)
   ;; This class is the legacy flat 0.8 compatibility model. New code should
   ;; validate raw JSON through VALIDATE-V090-DOCUMENT.
   (version :accessor doc-version
            :type string
            :initform +legacy-starintel-doc-version+)
   (date-updated :accessor doc-updated
                 :type integer
                 :initarg :date-updated
                 :initform (unix-now))
   (date-added :accessor doc-added
               :type integer
               :initarg :date-added
               :initform (unix-now))))

(defgeneric ulid-id (document)
  (:documentation "Generate a ULID for the document."))
(defgeneric timestamp (document)
  (:documentation "Set the document's date-added and date-updated fields to the current Unix time."))
(defgeneric update-timetamp (document)
  (:documentation "Update the document's date-updated field to the current Unix time."))
(defgeneric hash-id (document &rest data)
  (:documentation "Generate a hash-based ID for the document."))
(defgeneric set-id (document)
  (:documentation "Set the document ID if it is not already set."))
(defgeneric set-type (document)
  (:documentation "Set the legacy document type based on its class name."))
(defgeneric set-meta (document dataset)
  (:documentation "Set legacy flat document metadata."))

(defmethod ulid-id ((doc document))
  (setf (doc-id doc) (cms-ulid:ulid)))

(defmethod timestamp ((doc document))
  (when (not (doc-added doc))
    (setf (doc-added doc) (unix-now)))
  (when (not (doc-updated doc))
    (setf (doc-updated doc) (unix-now))))

(defmethod update-timetamp ((doc document))
  (setf (doc-updated doc) (unix-now)))

(defmethod hash-id ((doc document) &rest data)
  (setf (doc-id doc)
        (ironclad:byte-array-to-hex-string
         (ironclad:digest-sequence
          *default-hash-algo*
          (ironclad:ascii-string-to-byte-array (format nil "~{~a~}" data))))))

(defmethod set-type ((doc document))
  (let* ((full-type (type-of doc))
         (type-parts (uiop:split-string (symbol-name full-type) :separator ":"))
         (type-name (car (last type-parts))))
    (setf (doc-type doc) (string-downcase type-name))))

(defmethod set-meta ((doc document) dataset)
  (setf (doc-dataset doc) dataset)
  (set-type doc)
  (when (or (not (doc-id doc)) (= (length (doc-id doc)) 0))
    (set-id doc))
  doc)
