(in-package :starintel)

;; Do not mask a live pre-migration CLOS package during a hot reload. A fresh
;; process (or explicit legacy namespace) is required for a truthful root API.
(when (or (member "SPEC" (package-nicknames *package*) :test #'string=)
          (find-class 'document nil))
  (error "Restart before loading canonical STARINTEL over the historical root package."))

(defparameter +starintel-doc-version+ (starintel.canonical:schema-version))
(defparameter +starintel-schema-version+ (starintel.canonical:schema-version))
(defparameter +starintel-release-version+ (starintel.canonical:release-version))

(defun release-version () (starintel.canonical:release-version))
(defun schema-version () (starintel.canonical:schema-version))
(defun document-types () (starintel.canonical:document-types))
(defun validate-document (value) (starintel.canonical:validate-document value))
(defun encode-document (value) (starintel.canonical:encode-document value))
(defun decode-document (value)
  (starintel.canonical:decode-document
   (if (stringp value) (parse-json value) value)))
(defun to-json (value) (stringify-json (encode-document value)))
(defun from-json (value) (decode-document value))
(defun doc-id (value) (gethash "id" (encode-document value)))

(defun create (dtype &rest fields)
  "Create a generated canonical document. Required values are never fabricated."
  (let* ((name (string-downcase (string dtype)))
         (schema (starintel.canonical:load-schema))
         (definition (starintel.canonical::document-definition name schema))
         (properties (gethash "properties" definition))
         (document (make-hash-table :test 'equal)))
    (unless (evenp (length fields)) (error "Expected canonical keyword/value pairs"))
    (setf (gethash "dtype" document) name
          (gethash "schemaVersion" document) (schema-version))
    (loop for (key value) on fields by #'cddr
          for wire = (loop for candidate being the hash-keys of properties
                           when (string-equal (string key) candidate) return candidate)
          do (unless wire (error "Unknown canonical field ~s" key))
             (when (nth-value 1 (gethash wire document))
               (error "Duplicate or reserved canonical field ~s" key))
             (setf (gethash wire document) (starintel.canonical::encode-generated-value value)))
    (decode-document document)))

(export '(+starintel-doc-version+ +starintel-schema-version+ +starintel-release-version+ release-version schema-version document-types validate-document
          encode-document decode-document to-json from-json doc-id create))

;; The public family constructor functions use generated types, not historical
;; CLOS classes. Historical accessors/classes are only in STARINTEL.LEGACY.
(dolist (dtype (document-types))
  (let ((name dtype)
        (symbol (intern (string-upcase dtype) :starintel)))
    (setf (symbol-function symbol)
          (lambda (&rest fields) (apply #'create name fields)))
    (export symbol :starintel)))
