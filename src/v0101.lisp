(in-package :starintel.canonical)

(defvar *release-lock* nil)
(defvar *manifest* nil)
(defvar *schema* nil)

(defun release-path (relative)
  (asdf:system-relative-pathname :starintel-0101
                                (concatenate 'string "schemas/starintel-0.10.1/" relative)))

(defun load-release-json (relative)
  (com.inuoe.jzon:parse (release-path relative)))

(defun release-version ()
  (gethash "releaseVersion" (or *release-lock* (setf *release-lock* (load-release-json "release-lock.json")))))

(defun schema-version ()
  (gethash "schemaVersion" (or *release-lock* (setf *release-lock* (load-release-json "release-lock.json")))))

(defun schema-path () (release-path "generated/schema.json"))
(defun load-schema () (or *schema* (setf *schema* (com.inuoe.jzon:parse (schema-path)))))

(defun portable-manifest ()
  (or *manifest* (setf *manifest* (load-release-json "generated/portable-manifest.json"))))

(defun document-contracts ()
  (remove-if-not (lambda (entry) (equal (gethash "kind" entry) "document"))
                 (coerce (gethash "types" (portable-manifest)) 'list)))

(defun short-contract-name (entry)
  (let ((name (gethash "name" entry)))
    (subseq name (1+ (position #\/ name :from-end t)))))

(defun document-types ()
  (sort (mapcar #'short-contract-name (document-contracts)) #'string<))

(defun definition-name (dtype)
  (format nil "~{~a~}" (mapcar #'string-capitalize (uiop:split-string dtype :separator "-"))))

(defun document-definition (dtype schema)
  (unless (member dtype (document-types) :test #'equal)
    (starintel::reject-document "unknown_object_type" "$.dtype: unknown document type ~s" dtype))
  (or (gethash (definition-name dtype) (gethash "$defs" schema))
      (error "Generated schema is missing document definition ~a" dtype)))

(defun validate-portable-scalar (value definition path)
  "Apply decimal bounds/scale carried by the generated portable manifest.

JSON Schema represents StarLang decimals as strings and cannot express numeric
bounds on them. Read these constraints from StarLang rather than redeclaring
latitude, confidence, or distance policy in this consumer."
  (let ((contract
          (find definition (coerce (gethash "types" (portable-manifest)) 'list)
                :test #'equal :key (lambda (entry) (definition-name (short-contract-name entry))))))
    (when (and contract (equal (gethash "kind" contract) "scalar")
               (equal (gethash "base" contract) "decimal"))
      (let* ((point (position #\. value))
             (digits (if point (- (length value) point 1) 0))
             (number (/ (parse-integer (remove #\. value)) (expt 10 digits))))
        (when (and (gethash "scale" contract) (> digits (gethash "scale" contract)))
          (starintel::reject-document "invalid_scale" "~a: decimal exceeds the generated scale" path))
        (when (and (gethash "minimum" contract) (< number (gethash "minimum" contract)))
          (starintel::reject-document "below_minimum" "~a: decimal is below minimum" path))
        (when (and (gethash "maximum" contract) (> number (gethash "maximum" contract)))
          (starintel::reject-document "above_maximum" "~a: decimal is above maximum" path))))))

(defun validate-document (document &optional (schema (load-schema)))
  "Validate a flat canonical document against the locked generated contract.

The generated schema is a library of definitions: validating its root alone
would accept everything. Select an exact document contract from the portable
manifest, then resolve all referenced scalar/enum/reference definitions."
  (unless (hash-table-p document)
    (starintel::reject-document "wrong_type" "$: expected object"))
  (unless (equal (gethash "schemaVersion" document) (schema-version))
    (starintel::reject-document "unsupported_spec_version" "$.schemaVersion: unsupported version"))
  (let ((starintel::*validation-schema-root* schema)
        (starintel::*validation-ref-hook* #'validate-portable-scalar))
    (starintel::validate-v090-value document (document-definition (gethash "dtype" document) schema)))
  document)

(defun roundtrip-document (document)
  (validate-document document)
  (let ((decoded (com.inuoe.jzon:parse (com.inuoe.jzon:stringify document))))
    (validate-document decoded)
    decoded))

(defun generated-wire-fields (object)
  (let* ((name (type-of object))
         (symbol (and (symbolp name)
                      (find-symbol (format nil "+~a-WIRE-FIELDS+" name)
                                   :org.starintel.core.v1))))
    (unless (and symbol (boundp symbol))
      (error "Not a generated StarIntel boundary value: ~s" object))
    (symbol-value symbol)))

(defun encode-generated-value (value)
  (typecase value
    (hash-table
     (let ((object (make-hash-table :test #'equal)))
       (maphash (lambda (key item) (setf (gethash key object) (encode-generated-value item))) value)
       object))
    (structure-object
     (let ((object (make-hash-table :test #'equal)))
       (dolist (field (generated-wire-fields value))
         (let ((item (slot-value value (cdr field))))
           ;; Generated optional slots use NIL for absence. False is explicit
           ;; through Jzon's NIL boolean in a hash-table input.
           (when item
             (setf (gethash (car field) object) (encode-generated-value item)))))
       object))
    (string value)
    (vector (map 'vector #'encode-generated-value value))
    (cons (map 'vector #'encode-generated-value value))
    (t (if (eq value :false) nil value))))

(defun encode-document (document)
  "Return validated canonical JSON data from a generated struct or hash-table."
  (validate-document (encode-generated-value document)))

(defun decode-document (document)
  "Validate JSON data and instantiate the generated Common Lisp document type."
  (validate-document document)
  (let* ((dtype (gethash "dtype" document))
         (constructor (find-symbol (string-upcase (concatenate 'string "MAKE-" dtype))
                                   :org.starintel.core.v1))
         (fields-symbol (find-symbol (string-upcase (format nil "+~a-WIRE-FIELDS+" dtype))
                                     :org.starintel.core.v1))
         (fields (symbol-value fields-symbol)))
    (apply constructor
           (loop for (wire . slot) in fields
                 when (nth-value 1 (gethash wire document))
                   append (list (intern (symbol-name slot) :keyword)
                                (or (gethash wire document) :false))))))
