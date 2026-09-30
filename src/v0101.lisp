(in-package :starintel-v0101)

(defparameter +spec-version+ "0.10.1")
(defparameter +adapter-version+ 2)

(define-condition starintel-validation-error (error)
  ((category :initarg :category :reader validation-category)
   (message :initarg :message :reader validation-message))
  (:report (lambda (condition stream)
             (format stream "~a: ~a"
                     (validation-category condition)
                     (validation-message condition)))))

(defun reject-document (category control &rest arguments)
  (error 'starintel-validation-error
         :category category
         :message (apply #'format nil control arguments)))

(defun json-object (&rest pairs)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun hash-present-p (object key)
  (and (hash-table-p object) (nth-value 1 (gethash key object))))

(defun hash-value (object key &optional default)
  (if (hash-table-p object)
      (multiple-value-bind (value presentp) (gethash key object)
        (if presentp value default))
      default))

(defun resource-path (name)
  (let ((root (uiop:getenv "STARINTEL_CONFORMANCE_ROOT")))
    (if root
        (merge-pathnames name (uiop:ensure-directory-pathname root))
        (asdf:system-relative-pathname
         :starintel/v0101
         (merge-pathnames name #P"../")))))

(defun load-json-resource (name)
  (let ((path (resource-path name)))
    (unless (probe-file path)
      (error "StarIntel resource not found: ~a" path))
    (com.inuoe.jzon:parse path)))

(defun load-schema ()
  (load-json-resource "schema/starintel-0.10.1.schema.json"))

(defun load-manifest ()
  (load-json-resource "schema/starintel-0.10.1.manifest.json"))

(defun load-compatibility ()
  (load-json-resource "schema/starintel-0.10.1.compatibility.json"))

(defun load-fixtures ()
  (load-json-resource "schema/starintel-0.10.1.compatibility-fixtures.json"))

(defun json-type-name (value)
  (cond
    ((eq value 'null) "null")
    ((or (eq value t) (null value)) "boolean")
    ((integerp value) "integer")
    ((numberp value) "number")
    ((stringp value) "string")
    ((vectorp value) "array")
    ((hash-table-p value) "object")
    (t (string-downcase (symbol-name (type-of value))))))

(defun matches-json-type-p (value expected)
  (cond
    ((string= expected "null") (eq value 'null))
    ((string= expected "boolean") (or (eq value t) (null value)))
    ((string= expected "integer") (integerp value))
    ((string= expected "number") (numberp value))
    ((string= expected "string") (stringp value))
    ((string= expected "array") (vectorp value))
    ((string= expected "object") (hash-table-p value))
    (t nil)))

(defun vector-member-equalp (value vector)
  (loop for item across vector thereis (equalp value item)))

(defun resolve-reference (schema reference)
  (let ((prefix "#/$defs/"))
    (unless (and (stringp reference)
                 (<= (length prefix) (length reference))
                 (string= prefix reference :end2 (length prefix)))
      (reject-document "invalid_schema_reference" "unsupported schema reference ~a" reference))
    (or (hash-value (hash-value schema "$defs") (subseq reference (length prefix)))
        (reject-document "invalid_schema_reference" "unknown schema reference ~a" reference))))

(defun valid-format-p (value format)
  (cond
    ((string= format "date")
     (cl-ppcre:scan "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" value))
    ((string= format "date-time")
     (cl-ppcre:scan
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})$"
      value))
    (t t)))

(defun validate-schema-value (value definition schema &optional (path "$"))
  (unless (hash-table-p definition)
    (return-from validate-schema-value t))

  (when (hash-present-p definition "$ref")
    (return-from validate-schema-value
      (validate-schema-value value
                             (resolve-reference schema (hash-value definition "$ref"))
                             schema path)))

  (when (hash-present-p definition "allOf")
    (loop for branch across (hash-value definition "allOf")
          do (validate-schema-value value branch schema path)))

  (when (hash-present-p definition "enum")
    (unless (vector-member-equalp value (hash-value definition "enum"))
      (reject-document "invalid_enum" "~a: value is not in enum" path)))

  (when (hash-present-p definition "type")
    (let ((expected (hash-value definition "type")))
      (unless (matches-json-type-p value expected)
        (reject-document "wrong_type" "~a: expected ~a, got ~a"
                         path expected (json-type-name value)))))

  (when (stringp value)
    (when (and (hash-present-p definition "format")
               (not (valid-format-p value (hash-value definition "format"))))
      (reject-document "invalid_format" "~a: invalid ~a"
                       path (hash-value definition "format")))
    (when (and (hash-present-p definition "pattern")
               (not (cl-ppcre:scan (hash-value definition "pattern") value)))
      (reject-document "pattern_mismatch" "~a: string does not match pattern" path)))

  (when (numberp value)
    (when (and (hash-present-p definition "minimum")
               (< value (hash-value definition "minimum")))
      (reject-document "below_minimum" "~a: number is below minimum" path))
    (when (and (hash-present-p definition "maximum")
               (> value (hash-value definition "maximum")))
      (reject-document "above_maximum" "~a: number is above maximum" path)))

  (when (and (vectorp value) (hash-present-p definition "items"))
    (loop for item across value
          for index from 0
          do (validate-schema-value item (hash-value definition "items") schema
                                    (format nil "~a[~d]" path index))))

  (when (hash-table-p value)
    (let ((properties (or (hash-value definition "properties")
                          (make-hash-table :test #'equal))))
      (when (hash-present-p definition "required")
        (loop for required across (hash-value definition "required")
              unless (hash-present-p value required)
                do (reject-document "missing_required_field"
                                    "~a: missing required field ~a" path required)))
      (let ((additional-present (hash-present-p definition "additionalProperties"))
            (additional (hash-value definition "additionalProperties" t)))
        (maphash
         (lambda (key item)
           (cond
             ((hash-present-p properties key)
              (validate-schema-value item (hash-value properties key) schema
                                     (format nil "~a.~a" path key)))
             ((and additional-present (hash-table-p additional))
              (validate-schema-value item additional schema (format nil "~a.~a" path key)))
             ((and additional-present (null additional))
              (reject-document "undeclared_field" "~a: undeclared field ~a" path key))))
         value))))
  t)

(defun compact-name (value)
  (remove #\- (string-downcase value)))

(defun dtype-definition (schema dtype)
  (let ((target (compact-name dtype))
        (found nil))
    (maphash (lambda (name definition)
               (when (string= target (compact-name name))
                 (setf found definition)))
             (hash-value schema "$defs"))
    found))

(defun decimal-value (value path)
  (unless (and (stringp value)
               (cl-ppcre:scan "^[+-]?[0-9]+(?:\\.[0-9]+)?$" value))
    (reject-document "wrong_type" "~a: expected decimal string" path))
  (multiple-value-bind (number position) (read-from-string value nil nil)
    (unless (and (numberp number) (= position (length value)))
      (reject-document "wrong_type" "~a: expected decimal string" path))
    number))

(defun validate-geo-range (document)
  (when (string= (hash-value document "dtype" "") "geo-point")
    (let ((longitude (decimal-value (hash-value document "longitude") "$.longitude"))
          (latitude (decimal-value (hash-value document "latitude") "$.latitude")))
      (unless (<= -180 longitude 180)
        (reject-document "above_maximum" "$.longitude: outside -180..180"))
      (unless (<= -90 latitude 90)
        (reject-document "above_maximum" "$.latitude: outside -90..90")))))

(defun validate-document (document &optional (schema (load-schema)))
  (unless (hash-table-p document)
    (reject-document "wrong_type" "$: expected object"))
  (unless (and (stringp (hash-value document "schemaVersion"))
               (string= (hash-value document "schemaVersion") +spec-version+))
    (reject-document "unsupported_spec_version"
                     "$.schemaVersion: expected ~a" +spec-version+))
  (let* ((dtype (hash-value document "dtype"))
         (definition (and (stringp dtype) (dtype-definition schema dtype))))
    (unless definition
      (reject-document "unknown_object_type" "$.dtype: unknown document type ~a" dtype))
    (validate-schema-value document definition schema)
    (validate-geo-range document)
    document))

(defun roundtrip-document (document &optional (schema (load-schema)))
  (validate-document document schema)
  (let ((copy (com.inuoe.jzon:parse (com.inuoe.jzon:stringify document))))
    (validate-document copy schema)
    copy))

(defun document-types (&optional (manifest (load-manifest)))
  (let ((types nil))
    (loop for contract across (hash-value manifest "types")
          when (string= (hash-value contract "kind" "") "document")
            do (let* ((name (hash-value contract "name"))
                      (slash (position #\/ name :from-end t)))
                 (push (subseq name (1+ slash)) types)))
    (coerce (sort types #'string<) 'vector)))

(defun capabilities ()
  (json-object
   "language" "cl"
   "adapterVersion" +adapter-version+
   "specVersions" (vector "0.9.0" +spec-version+)
   "emittedSpecVersion" +spec-version+
   "objectTypes" (document-types)
   "canonicalKeyStyle" "lowerCamelCase"
   "generatedPackage" "ORG.STARINTEL.CORE.V1"))
