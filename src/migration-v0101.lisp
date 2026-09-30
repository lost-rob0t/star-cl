(in-package :starintel-v0101)

(define-condition migration-error (error)
  ((reason-code :initarg :reason-code :reader migration-reason-code)
   (message :initarg :message :reader migration-message))
  (:report (lambda (condition stream)
             (format stream "~a: ~a"
                     (migration-reason-code condition)
                     (migration-message condition)))))

(defun reject-migration (reason-code control &rest arguments)
  (error 'migration-error
         :reason-code reason-code
         :message (apply #'format nil control arguments)))

(defun clone-json (value)
  (com.inuoe.jzon:parse (com.inuoe.jzon:stringify value)))

(defun camel-key (name)
  (with-output-to-string (stream)
    (loop with uppercase-next = nil
          for character across name
          do (cond
               ((char= character #\_)
                (setf uppercase-next t))
               (uppercase-next
                (write-char (char-upcase character) stream)
                (setf uppercase-next nil))
               (t (write-char character stream))))))

(defun vector-contains-string-p (vector value)
  (loop for item across vector thereis (string= item value)))

(defun normalized-object (value aliases opaque-fields)
  (let ((result (make-hash-table :test #'equal)))
    (maphash
     (lambda (source item)
       (let ((target (or (hash-value aliases source) (camel-key source))))
         (when (hash-present-p result target)
           (reject-migration "ambiguousFieldCollision"
                             "multiple fields normalize to ~a" target))
         (setf (gethash target result)
               (cond
                 ((vector-contains-string-p opaque-fields target) item)
                 ((stringp item) item)
                 ((hash-table-p item) (normalized-object item aliases opaque-fields))
                 ((vectorp item)
                  (map 'vector
                       (lambda (entry)
                         (if (hash-table-p entry)
                             (normalized-object entry aliases opaque-fields)
                             entry))
                       item))
                 (t item)))))
     value)
    result))

(defun merge-legacy-data (document)
  (when (hash-present-p document "data")
    (let ((data (hash-value document "data")))
      (remhash "data" document)
      (unless (hash-table-p data)
        (reject-migration "migrationFailed" "legacy data must be an object"))
      (maphash
       (lambda (key value)
         (when (hash-present-p document key)
           (reject-migration "ambiguousFieldCollision"
                             "legacy data collides at ~a" key))
         (setf (gethash key document) value))
       data))))

(defun apply-dtype-alias (document compatibility)
  (let* ((dtype (hash-value document "dtype"))
         (alias (and (stringp dtype)
                     (hash-value (hash-value compatibility "dtypeAliases") dtype))))
    (when alias
      (setf (gethash "dtype" document) alias))))

(defun string-prefix-p (prefix value)
  (and (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

(defun classify-media (document compatibility)
  (let ((policy (hash-value compatibility "mediaClassification")))
    (when (string= (hash-value document "dtype" "")
                   (hash-value policy "legacyDtype"))
      (let ((media-type nil))
        (loop for field across (hash-value policy "contentTypePrecedence")
              for value = (hash-value document field)
              when (and (stringp value) (plusp (length value)))
                do (setf media-type (string-downcase value)) (loop-finish))
        (setf (gethash "dtype" document) (hash-value policy "fallbackDtype"))
        (when media-type
          (loop for rule across (hash-value policy "rules")
                when (string-prefix-p (hash-value rule "prefix") media-type)
                  do (setf (gethash "dtype" document) (hash-value rule "dtype"))
                     (loop-finish)))))))

(defun convert-geo (document compatibility)
  (let ((policy (hash-value compatibility "geo")))
    (when (string= (hash-value document "dtype" "")
                   (hash-value policy "legacyDtype"))
      (maphash
       (lambda (source target)
         (let ((normalized-source (camel-key source)))
           (when (hash-present-p document normalized-source)
             (when (hash-present-p document target)
               (reject-migration "ambiguousFieldCollision"
                                 "geo field collides at ~a" target))
             (setf (gethash target document) (hash-value document normalized-source))
             (remhash normalized-source document))))
       (hash-value policy "fieldAliases"))
      (setf (gethash "dtype" document) (hash-value policy "canonicalDtype"))
      (maphash (lambda (key value)
                 (unless (hash-present-p document key)
                   (setf (gethash key document) value)))
               (hash-value policy "defaults"))
      (handler-case
          (validate-geo-range document)
        (starintel-validation-error (condition)
          (reject-migration "canonicalValidationFailed" "~a" condition))))))

(defun reference (dtype id)
  (json-object "schema" (format nil "org.starintel/core@1/~a" dtype)
               "id" id))

(defun digest-id (prefix parts)
  (let ((payload
          (with-output-to-string (stream)
            (write-string prefix stream)
            (dolist (part parts)
              (write-char #\Null stream)
              (write-string part stream)))))
    (string-downcase
     (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence
       :sha256
       (babel:string-to-octets payload :encoding :utf-8))))))

(defun sortable-hash-keys (object)
  (sort (loop for key being the hash-keys of object collect key) #'string<))

(defun extract-person-identifiers (document)
  (let ((external-ids (hash-value document "externalIds")))
    (unless (and (string= (hash-value document "dtype" "") "person")
                 (hash-table-p external-ids))
      (return-from extract-person-identifiers #()))
    (let ((generated nil)
          (references nil))
      (dolist (scheme (sortable-hash-keys external-ids))
        (let ((raw-value (hash-value external-ids scheme)))
          (when (or (stringp raw-value) (numberp raw-value))
            (let* ((value (if (stringp raw-value)
                              raw-value
                              (princ-to-string raw-value)))
                   (normalized-value
                     (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
                   (id (format nil "starintel:person-identifier:~a"
                               (digest-id "personIdentifier"
                                          (list (hash-value document "id")
                                                scheme normalized-value)))))
              (push (reference "person-identifier" id) references)
              (push (json-object
                     "id" id
                     "dataset" (hash-value document "dataset")
                     "dtype" "person-identifier"
                     "schemaVersion" +spec-version+
                     "person" (reference "person" (hash-value document "id"))
                     "scheme" scheme
                     "value" value
                     "normalizedValue" normalized-value)
                    generated)))))
      (when references
        (setf (gethash "identifiers" document) (coerce (nreverse references) 'vector)))
      (coerce (nreverse generated) 'vector))))

(defun extract-transcript (document)
  (let ((text nil))
    (dolist (field '("transcript" "transcriptText"))
      (let ((value (hash-value document field)))
        (when (and (null text) (stringp value) (plusp (length value)))
          (setf text value)
          (remhash field document))))
    (unless text
      (return-from extract-transcript #()))
    (let* ((language (or (hash-value document "language") ""))
           (dtype (hash-value document "dtype"))
           (id (format nil "starintel:transcript:~a"
                       (digest-id "transcript"
                                  (list (hash-value document "id") language))))
           (transcript-reference (reference "transcript" id))
           (transcript
             (json-object
              "id" id
              "dataset" (hash-value document "dataset")
              "dtype" "transcript"
              "schemaVersion" +spec-version+
              "sourceMedia" (reference dtype (hash-value document "id"))
              "text" text)))
      (if (string= dtype "audio")
          (setf (gethash "transcripts" document) (vector transcript-reference))
          (setf (gethash "transcript" document) transcript-reference))
      (when (plusp (length language))
        (setf (gethash "language" transcript) language))
      (vector transcript))))

(defun preserve-unknown (document schema)
  (let* ((definition (dtype-definition schema (hash-value document "dtype" "")))
         (properties (and definition (hash-value definition "properties"))))
    (unless properties
      (reject-migration "canonicalValidationFailed" "unknown dtype ~a"
                        (hash-value document "dtype")))
    (let ((unknown (make-hash-table :test #'equal))
          (unknown-keys nil))
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (unless (hash-present-p properties key)
                   (push key unknown-keys)))
               document)
      (dolist (key unknown-keys)
        (setf (gethash key unknown) (hash-value document key))
        (remhash key document))
      (when (plusp (hash-table-count unknown))
        (let ((extensions (hash-value document "extensions")))
          (cond
            ((null extensions)
             (setf extensions (make-hash-table :test #'equal)
                   (gethash "extensions" document) extensions))
            ((not (hash-table-p extensions))
             (reject-migration "ambiguousFieldCollision" "extensions is not an object")))
          (when (hash-present-p extensions "legacy")
            (reject-migration "ambiguousFieldCollision" "extensions.legacy collides"))
          (setf (gethash "legacy" extensions) unknown))))))

(defun append-vectors (&rest vectors)
  (coerce (loop for vector in vectors append (coerce vector 'list)) 'vector))

(defun migrate-document (value &optional
                                 (compatibility (load-compatibility))
                                 (schema (load-schema)))
  (unless (hash-table-p value)
    (reject-migration "decodeFailed" "document must be an object"))
  (let* ((legacy (hash-value compatibility "legacyInput"))
         (document
           (normalized-object (clone-json value)
                              (hash-value legacy "envelopeAliases")
                              (hash-value legacy "opaqueMapFields")))
         (accepted (hash-value compatibility "acceptedSchemaVersions")))
    (unless (and (stringp (hash-value document "schemaVersion"))
                 (vector-member-equalp (hash-value document "schemaVersion") accepted))
      (reject-migration "unsupportedSchemaVersion" "unsupported schema version ~a"
                        (hash-value document "schemaVersion")))
    (merge-legacy-data document)
    (apply-dtype-alias document compatibility)
    (classify-media document compatibility)
    (convert-geo document compatibility)
    (setf (gethash "schemaVersion" document) +spec-version+)
    (let ((documents
            (append-vectors (vector document)
                            (extract-person-identifiers document)
                            (extract-transcript document))))
      (loop for migrated across documents
            do (preserve-unknown migrated schema)
               (handler-case
                   (validate-document migrated schema)
                 (starintel-validation-error (condition)
                   (reject-migration "canonicalValidationFailed" "~a" condition))))
      documents)))

(defun migrate-batch (values &optional
                              (compatibility (load-compatibility))
                              (schema (load-schema)))
  (let ((documents nil)
        (quarantine nil)
        (seen (make-hash-table :test #'equal)))
    (loop for value across values
          do (handler-case
                 (loop for document across
                         (migrate-document value compatibility schema)
                       for id = (hash-value document "id")
                       unless (and id (hash-present-p seen id))
                         do (when id (setf (gethash id seen) t))
                            (push document documents))
               (migration-error (condition)
                 (push (json-object "reasonCode"
                                    (migration-reason-code condition))
                       quarantine))
               (error ()
                 (push (json-object "reasonCode" "migrationFailed") quarantine))))
    (json-object "documents" (coerce (nreverse documents) 'vector)
                 "quarantine" (coerce (nreverse quarantine) 'vector))))
