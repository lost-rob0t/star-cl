(in-package :starintel)

(defparameter *starintel-doc-version* "0.7.1")

(defun unix-now ()
  (- (local-time:timestamp-to-universal (local-time:now))
     (encode-universal-time 0 0 0 1 1 1970 0)))



(defclass document ()
  ((_id :accessor doc-id :type string :initarg :id :initform "")
   (dataset :accessor doc-dataset :type string :initarg :dataset :initform "")
   (dtype :accessor doc-type :type string :initarg :dtype :initform "")
   (sources :accessor doc-sources :type list :initform nil)
   (version :accessor doc-version :type integer :initform *starintel-doc-version*)
   (date-updated :accessor doc-updated :type integer :initarg :date-updated :initform (unix-now))
   (date-added :accessor doc-added :type integer :initarg :date-added :initform (unix-now))))


;; Not really needed but itsfor my own santiy from documents.nim

(defmethod make-uuid ((doc document))
  "Create a UUID for the document"
  (setf (doc-id doc) (uuid:make-v4-uuid)))

(defmethod timestamp ((doc document))
  "Add the current time in unix to the document"
  (setf (doc-added doc) (unix-now))
  (setf (doc-updated doc) (unix-now)))

(defmethod update-timetamp ((doc document))
  "Update the doc_updated field to the current unix epoch time."
  (setf (doc-updated doc) (unix-now)))

(defmethod hash-id ((doc document) data &optional (type :md5))
  (setf (doc-id doc) (ironclad:byte-array-to-hex-string (ironclad:digest-sequence
                                                         type
                                                         (ironclad:ascii-string-to-byte-array data)))))





(defgeneric set-type (doc)
  (:documentation "Set the type of a Booker document"))

(defmethod set-type ((doc document))
  (let* ((full-type (type-of doc))
         (type-parts (uiop:split-string (symbol-name full-type) :separator ":"))
         (type-name (car (last type-parts))))
    (setf (doc-type doc) (string-downcase type-name))))


(defgeneric set-meta (doc dataset)
  (:documentation "Set the metadata of a Booker document"))

(defmethod set-meta ((doc document) dataset)
  (setf (doc-dataset doc) dataset)
  (timestamp doc)
  (set-type doc)
  doc)
