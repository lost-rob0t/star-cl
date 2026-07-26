(in-package :starintel)

(defparameter +starintel-schema-version+ "0.9.0")
(defparameter +starintel-adapter-version+ 1)

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
  (nth-value 1 (gethash key object)))

(defun hash-value (object key &optional default)
  (multiple-value-bind (value presentp) (gethash key object)
    (if presentp value default)))

(defun schema-path ()
  (or (uiop:getenv "STARINTEL_SCHEMA")
      (let ((root (uiop:getenv "STARINTEL_CONFORMANCE_ROOT")))
        (when root
          (namestring
           (merge-pathnames "schemas/starintel-doc-v0.9.0.schema.json"
                            (uiop:ensure-directory-pathname root)))))
      (namestring
       (merge-pathnames "schemas/starintel-doc-v0.9.0.schema.json"
                        (uiop:getcwd)))))

(defun load-v090-schema ()
  (let ((path (schema-path)))
    (unless (probe-file path)
      (error "StarIntel schema not found: ~a" path))
    (com.inuoe.jzon:parse (pathname path))))

(defun json-type-name (value)
  (cond
    ((eq value 'null) "null")
    ((or (eq value t) (null value)) "boolean")
    ((integerp value) "integer")
    ((floatp value) "number")
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
    (t t)))

(defun valid-date-time-p (value)
  (and (stringp value)
       (cl-ppcre:scan
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})$"
        value)))

(defun vector-member-equalp (value vector)
  (loop for item across vector thereis (equalp value item)))

(defun validate-any-of (value candidates path)
  (loop for candidate across candidates
        do (handler-case
               (progn
                 (validate-v090-value value candidate path)
                 (return-from validate-any-of t))
             (starintel-validation-error () nil)))
  (reject-document "wrong_type" "~a: value did not match any allowed schema" path))

(defun validate-object (value schema path)
  (let ((properties (or (hash-value schema "properties")
                        (make-hash-table :test #'equal))))
    (when (hash-present-p schema "required")
      (loop for required across (hash-value schema "required")
            unless (hash-present-p value required)
              do (reject-document "missing_required_field"
                                  "~a: missing required field ~a"
                                  path required)))
    (let* ((additional-present (hash-present-p schema "additionalProperties"))
           (additional (hash-value schema "additionalProperties" t)))
      (maphash
       (lambda (key item)
         (cond
           ((hash-present-p properties key)
            (validate-v090-value item
                                 (hash-value properties key)
                                 (format nil "~a.~a" path key)))
           ((and additional-present (hash-table-p additional))
            (validate-v090-value item additional (format nil "~a.~a" path key)))
           ((and additional-present (null additional))
            (reject-document "undeclared_field"
                             "~a: undeclared field ~a"
                             path key))))
       value)))
  t)

(defun validate-all-of (value schema path)
  (when (hash-present-p schema "allOf")
    (loop for branch across (hash-value schema "allOf")
          for applies =
            (if (hash-present-p branch "if")
                (handler-case
                    (progn
                      (validate-v090-value value (hash-value branch "if") path)
                      t)
                  (starintel-validation-error () nil))
                t)
          when (and applies (hash-present-p branch "then"))
            do (validate-v090-value value (hash-value branch "then") path)))
  t)

(defun validate-v090-value (value schema &optional (path "$"))
  (unless (hash-table-p schema)
    (return-from validate-v090-value t))
  (when (zerop (hash-table-count schema))
    (return-from validate-v090-value t))

  (when (hash-present-p schema "anyOf")
    (return-from validate-v090-value
      (validate-any-of value (hash-value schema "anyOf") path)))

  (when (and (hash-present-p schema "const")
             (not (equalp value (hash-value schema "const"))))
    (reject-document "invalid_constant" "~a: unexpected constant" path))

  (when (and (hash-present-p schema "enum")
             (not (vector-member-equalp value (hash-value schema "enum"))))
    (reject-document "invalid_enum" "~a: value is not in enum" path))

  (when (hash-present-p schema "type")
    (let ((expected (hash-value schema "type")))
      (unless (matches-json-type-p value expected)
        (reject-document "wrong_type"
                         "~a: expected ~a, got ~a"
                         path expected (json-type-name value)))))

  (when (stringp value)
    (when (and (hash-present-p schema "format")
               (string= (hash-value schema "format") "date-time")
               (not (valid-date-time-p value)))
      (reject-document "invalid_datetime" "~a: invalid ISO-8601 date-time" path))
    (when (and (hash-present-p schema "pattern")
               (not (cl-ppcre:scan (hash-value schema "pattern") value)))
      (reject-document "pattern_mismatch" "~a: string does not match pattern" path)))

  (when (numberp value)
    (when (and (hash-present-p schema "minimum")
               (< value (hash-value schema "minimum")))
      (reject-document "below_minimum" "~a: number is below minimum" path))
    (when (and (hash-present-p schema "maximum")
               (> value (hash-value schema "maximum")))
      (reject-document "above_maximum" "~a: number is above maximum" path)))

  (when (and (vectorp value) (hash-present-p schema "items"))
    (loop for item across value
          for index from 0
          do (validate-v090-value item
                                  (hash-value schema "items")
                                  (format nil "~a[~d]" path index))))

  (when (hash-table-p value)
    (validate-object value schema path))

  (validate-all-of value schema path)
  t)

(defun v090-object-types (schema)
  (let ((result nil))
    (when (hash-present-p schema "allOf")
      (loop for branch across (hash-value schema "allOf")
            for if-schema = (hash-value branch "if")
            for properties = (and if-schema (hash-value if-schema "properties"))
            for dtype-schema = (and properties (hash-value properties "dtype"))
            for dtype = (and dtype-schema (hash-value dtype-schema "const"))
            when dtype do (push dtype result)))
    (sort result #'string<)))

(defun alias-dtype-p (dtype)
  (member dtype
          '("organization" "organisation" "investigation_target"
            "social_media_post" "email_message" "financial_observation"
            "research_pass" "dataset_manifest" "actor_manifest"
            "legal_case" "lobbying_filing" "campaign_finance")
          :test #'string=))

(defun validate-v090-document (document schema)
  (unless (hash-table-p document)
    (reject-document "wrong_type" "$: expected object"))
  (unless (and (hash-present-p document "schema_version")
               (stringp (hash-value document "schema_version"))
               (string= (hash-value document "schema_version")
                        +starintel-schema-version+))
    (reject-document "unsupported_spec_version"
                     "$.schema_version: unsupported version"))
  (when (and (hash-present-p document "dtype")
             (stringp (hash-value document "dtype")))
    (let ((dtype (hash-value document "dtype"))
          (types (v090-object-types schema)))
      (unless (member dtype types :test #'string=)
        (if (alias-dtype-p dtype)
            (reject-document "invalid_enum" "$.dtype: alias is not canonical")
            (reject-document "unknown_object_type" "$.dtype: unknown document type")))))
  (handler-case
      (validate-v090-value document schema)
    (starintel-validation-error (condition)
      (if (string= (validation-category condition) "invalid_constant")
          (reject-document "unsupported_spec_version"
                           "~a"
                           (validation-message condition))
          (error condition))))
  document)

(defun roundtrip-v090-document (document schema)
  (validate-v090-document document schema)
  (let* ((encoded (com.inuoe.jzon:stringify document))
         (decoded (com.inuoe.jzon:parse encoded)))
    (validate-v090-document decoded schema)
    decoded))

(defun definition-inventory (definition requiredp)
  (let ((item (json-object "required" requiredp)))
    (when (hash-present-p definition "type")
      (setf (gethash "type" item) (hash-value definition "type")))
    (when (hash-present-p definition "format")
      (setf (gethash "format" item) (hash-value definition "format")))
    (when (hash-present-p definition "enum")
      (setf (gethash "enum" item) (hash-value definition "enum")))
    (when (hash-present-p definition "anyOf")
      (setf (gethash "any_of" item)
            (map 'vector
                 (lambda (candidate)
                   (cond
                     ((hash-present-p candidate "type")
                      (hash-value candidate "type"))
                     ((hash-present-p candidate "const")
                      (hash-value candidate "const"))
                     (t "any")))
                 (hash-value definition "anyOf"))))
    item))

(defun v090-schema-inventory (schema)
  (let ((entries nil))
    (loop for branch across (hash-value schema "allOf")
          for dtype = (hash-value
                       (hash-value
                        (hash-value
                         (hash-value branch "if") "properties")
                        "dtype")
                       "const")
          for data-schema = (hash-value
                             (hash-value
                              (hash-value branch "then") "properties")
                             "data")
          do (push (cons dtype data-schema) entries))
    (setf entries (sort entries #'string< :key #'car))
    (coerce
     (mapcar
      (lambda (entry)
        (let* ((dtype (car entry))
               (data-schema (cdr entry))
               (required-vector (or (hash-value data-schema "required") #()))
               (required (coerce required-vector 'list))
               (fields (make-hash-table :test #'equal))
               (properties (or (hash-value data-schema "properties")
                               (make-hash-table :test #'equal))))
          (maphash
           (lambda (name definition)
             (setf (gethash name fields)
                   (definition-inventory
                    definition
                    (not (null (member name required :test #'string=))))))
           properties)
          (json-object "object_type" dtype "fields" fields)))
      entries)
     'vector)))

(defun v090-capabilities (schema)
  (json-object
   "language" "cl"
   "adapter_version" +starintel-adapter-version+
   "spec_versions" (vector +starintel-schema-version+)
   "commands" (vector "validate" "normalize" "roundtrip" "version"
                      "capabilities" "schema-inventory")
   "object_types" (coerce (v090-object-types schema) 'vector)
   "preserves_unknown_extensions" t
   "preserves_missing_optional_fields" t))
