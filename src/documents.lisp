(in-package :starintel)

(defparameter +starintel-doc-version+ "0.9.0")
(defparameter *default-hash-algo* :sha256)

(defparameter +document-envelope-slot-names+
  '(_id _rev dataset dtype schema-version version date-added date-updated
    title summary description status language tags labels aliases keywords
    identifiers sources evidence temporal provenance assessment verification
    handling lineage quality workflow geospatial attachments related-ids notes
    schema-org data extensions))

(defun document-envelope-slot-p (slot-name)
  (member slot-name +document-envelope-slot-names+ :test #'eq))

(defun unix-now ()
  (- (local-time:timestamp-to-universal (local-time:now))
     (encode-universal-time 0 0 0 1 1 1970 0)))

(defun utc-now ()
  (local-time:format-timestring nil (local-time:now)
                                :format local-time:+iso-8601-format+))

(defclass document ()
  ((_id :accessor doc-id
        :type string
        :initarg :id
        :initform "")
   (_rev :accessor doc-rev
         :type (or null string)
         :initarg :rev
         :initform nil)
   (dataset :accessor doc-dataset
            :type string
            :initarg :dataset
            :initform "star-intel")
   (dtype :accessor doc-type
          :type string
          :initarg :dtype
          :initform "document")
   (schema-version :accessor doc-schema-version
                   :type string
                   :initarg :schema-version
                   :initform +starintel-doc-version+)
   (version :accessor doc-version
            :type integer
            :initarg :version
            :initform 1)
   (date-added :accessor doc-added
               :type string
               :initarg :date-added
               :initform (utc-now))
   (date-updated :accessor doc-updated
                 :type string
                 :initarg :date-updated
                 :initform (utc-now))
   (title :accessor doc-title :type string :initarg :title :initform "")
   (summary :accessor doc-summary :type string :initarg :summary :initform "")
   (description :accessor doc-description :type string :initarg :description :initform "")
   (status :accessor doc-status :type string :initarg :status :initform "recorded")
   (language :accessor doc-language :type string :initarg :language :initform "en")
   (tags :accessor doc-tags :type list :initarg :tags :initform nil)
   (labels :accessor doc-labels :type list :initarg :labels :initform nil)
   (aliases :accessor doc-aliases :type list :initarg :aliases :initform nil)
   (keywords :accessor doc-keywords :type list :initarg :keywords :initform nil)
   (identifiers :accessor doc-identifiers :type list :initarg :identifiers :initform nil)
   (sources :accessor doc-sources :type list :initarg :sources :initform nil)
   (evidence :accessor doc-evidence :type list :initarg :evidence :initform nil)
   (temporal :accessor doc-temporal :type t :initarg :temporal :initform (jsown:empty-object))
   (provenance :accessor doc-provenance :type t :initarg :provenance :initform (jsown:empty-object))
   (assessment :accessor doc-assessment :type t :initarg :assessment :initform (jsown:empty-object))
   (verification :accessor doc-verification :type t :initarg :verification :initform (jsown:empty-object))
   (handling :accessor doc-handling :type t :initarg :handling :initform (jsown:empty-object))
   (lineage :accessor doc-lineage :type t :initarg :lineage :initform (jsown:empty-object))
   (quality :accessor doc-quality :type t :initarg :quality :initform (jsown:empty-object))
   (workflow :accessor doc-workflow :type t :initarg :workflow :initform (jsown:empty-object))
   (geospatial :accessor doc-geospatial :type t :initarg :geospatial :initform (jsown:empty-object))
   (attachments :accessor doc-attachments :type list :initarg :attachments :initform nil)
   (related-ids :accessor doc-related-ids :type list :initarg :related-ids :initform nil)
   (notes :accessor doc-notes :type list :initarg :notes :initform nil)
   (schema-org :accessor doc-schema-org :type t :initarg :schema-org :initform nil)
   (data :accessor doc-data :type t :initarg :data :initform (jsown:empty-object))
   (extensions :accessor doc-extensions :type t :initarg :extensions :initform (jsown:empty-object))))

(defgeneric ulid-id (document))
(defgeneric timestamp (document))
(defgeneric update-timetamp (document))
(defgeneric update-timestamp (document))
(defgeneric hash-id (document &rest data))
(defgeneric set-id (document))
(defgeneric set-type (document))
(defgeneric set-meta (document dataset))
(defgeneric refresh-schema-org (document))
(defgeneric touch (document &key updated-by))

(defmethod ulid-id ((doc document))
  (setf (doc-id doc) (cms-ulid:ulid))
  (refresh-schema-org doc)
  (doc-id doc))

(defmethod timestamp ((doc document))
  (let ((now (utc-now)))
    (when (or (null (doc-added doc)) (string= (doc-added doc) ""))
      (setf (doc-added doc) now))
    (when (or (null (doc-updated doc)) (string= (doc-updated doc) ""))
      (setf (doc-updated doc) now)))
  doc)

(defmethod update-timestamp ((doc document))
  (setf (doc-updated doc) (utc-now))
  doc)

(defmethod update-timetamp ((doc document))
  (update-timestamp doc))

(defmethod hash-id ((doc document) &rest data)
  (setf (doc-id doc)
        (ironclad:byte-array-to-hex-string
         (ironclad:digest-sequence
          *default-hash-algo*
          (ironclad:ascii-string-to-byte-array (format nil "~{~a~^~c~}" data #\Unit-Separator)))))
  (refresh-schema-org doc)
  (doc-id doc))

(defmethod set-id ((doc document))
  (when (or (null (doc-id doc)) (string= (doc-id doc) ""))
    (ulid-id doc))
  (doc-id doc))

(defmethod set-type ((doc document))
  (setf (doc-type doc)
        (canonical-dtype (string-downcase (symbol-name (class-name (class-of doc))))))
  (doc-type doc))

(defmethod refresh-schema-org ((doc document))
  (setf (doc-schema-org doc)
        (schema-org-metadata (doc-type doc) (doc-id doc) (doc-schema-org doc)))
  (doc-schema-org doc))

(defmethod set-meta ((doc document) dataset)
  (setf (doc-dataset doc) dataset
        (doc-schema-version doc) +starintel-doc-version+)
  (set-type doc)
  (set-id doc)
  (timestamp doc)
  (refresh-schema-org doc)
  doc)

(defmethod touch ((doc document) &key (updated-by ""))
  (incf (doc-version doc))
  (update-timestamp doc)
  (when (> (length updated-by) 0)
    (setf (jsown:val (doc-provenance doc) "updated_by") updated-by))
  (refresh-schema-org doc)
  doc)

(defmethod initialize-instance :after ((doc document) &key)
  (setf (doc-schema-version doc) +starintel-doc-version+)
  (set-type doc)
  (set-id doc)
  (timestamp doc)
  (refresh-schema-org doc))
