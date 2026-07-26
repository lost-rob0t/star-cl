(in-package :starintel-test)

(def-suite json-test
  :description "Typed JSON codec semantics"
  :in starintel-test)

(in-suite json-test)

(defclass codec-state-object ()
  ((flag
    :accessor codec-state-flag
    :type boolean
    :initarg :flag
    :initform nil)
   (items
    :accessor codec-state-items
    :type list
    :initarg :items
    :initform nil)
   (nullable
    :accessor codec-state-nullable
    :type (or null string)
    :initarg :nullable
    :initform nil)
   (empty-string
    :accessor codec-state-empty-string
    :type string
    :initarg :empty-string
    :initform "")))

(defclass codec-child ()
  ((name
    :accessor codec-child-name
    :type string
    :initarg :name
    :initform "")))

(defclass codec-parent ()
  ((child
    :accessor codec-parent-child
    :type codec-child
    :initarg :child)
   (counts
    :accessor codec-parent-counts
    :type list
    :initarg :counts
    :initform nil)))

(defclass codec-required-object ()
  ((name
    :accessor codec-required-name
    :type string
    :initarg :name)))

(defclass codec-default-object ()
  ((name
    :accessor codec-default-name
    :type string
    :initarg :name
    :initform "default")))

(defun injective-round-trip (object)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json object))))

(test encode-returns-jsown-object
  (let ((encoded (encode (make-instance 'codec-state-object))))
    (is (not (stringp encoded)))
    (is (equal encoded (injective-round-trip encoded)))))

(test encode-keeps-false-empty-array-null-and-empty-string-distinct
  (let* ((encoded (encode (make-instance 'codec-state-object)))
         (wire (injective-round-trip encoded)))
    (is (eq :false (jsown:val wire "flag")))
    (is (and (listp (jsown:val wire "items"))
             (null (jsown:val wire "items"))))
    (is (eq :null (jsown:val wire "nullable")))
    (is (string= "" (jsown:val wire "emptyString")))))

(test absent-fields-keep-initforms
  (let* ((json (jsown:empty-object))
         (decoded (decode json 'codec-default-object)))
    (is (string= "default" (codec-default-name decoded)))))

(test unbound-required-slots-fail-encoding
  (signals codec-validation-error
    (encode (make-instance 'codec-required-object))))

(test nested-objects-and-typed-collections-round-trip
  (let* ((parent
           (make-instance 'codec-parent
                          :child (make-instance 'codec-child :name "child")
                          :counts '(1 2 3)))
         (encoded (encode parent))
         (decoded (decode encoded 'codec-parent)))
    (is (string= "child"
                 (codec-child-name (codec-parent-child decoded))))
    (is (equal '(1 2 3) (codec-parent-counts decoded)))))

(test empty-typed-list-encodes-as-array
  (let* ((parent
           (make-instance 'codec-parent
                          :child (make-instance 'codec-child)
                          :counts nil))
         (wire (injective-round-trip (encode parent))))
    (is (and (listp (jsown:val wire "counts"))
             (null (jsown:val wire "counts"))))))

(test primitive-decoding-is-strict
  (signals codec-validation-error
    (decode-value "42" 'integer))
  (signals codec-validation-error
    (decode-value 1 'string))
  (signals codec-validation-error
    (decode-value "false" 'boolean))
  (signals codec-validation-error
    (decode-value :null 'string))
  (signals codec-validation-error
    (decode-value '("1") '(list integer))))

(test primitive-encoding-is-strict
  (signals codec-validation-error
    (encode-value "42" 'integer))
  (signals codec-validation-error
    (encode-value 1 'string))
  (signals codec-validation-error
    (encode-value "false" 'boolean))
  (signals codec-validation-error
    (encode-value :null 'string))
  (signals codec-validation-error
    (encode-value '("1") '(list integer))))

(test nullable-null-decodes-without-affecting-empty-string
  (is (null (decode-value :null '(or null string))))
  (is (string= "" (decode-value "" '(or null string)))))

(test boolean-decoding-does-not-use-truthiness
  (is (eq t (decode-value :true 'boolean)))
  (is (null (decode-value :false 'boolean)))
  (signals codec-validation-error
    (decode-value "anything" 'boolean)))

(test document-registry-selects-class-by-dtype
  (let* ((person (make-instance 'person :fname "Ada" :lname "Lovelace"))
         (encoded (encode person))
         (decoded (decode-document encoded)))
    (is (typep decoded 'person))
    (is (string= "Ada" (person-fname decoded)))
    (is (eq (find-class 'person)
            (registered-document-class "person")))))

(test document-registry-rejects-class-mismatch
  (let ((encoded
          (encode (make-instance 'person :fname "Ada" :lname "Lovelace"))))
    (signals document-class-mismatch
      (decode encoded 'org))))

(test document-registry-rejects-unknown-dtype-without-interning
  (let* ((unknown "codec-untrusted-dtype-42f1")
         (package (find-package :starintel))
         (encoded
           (encode (make-instance 'person :fname "Ada" :lname "Lovelace")))
         (before (multiple-value-list (find-symbol unknown package))))
    (setf (jsown:val encoded "dtype") unknown)
    (signals unknown-document-dtype
      (decode-document encoded))
    (is (equal before
               (multiple-value-list (find-symbol unknown package))))))

(test document-data-must-be-an-object
  (let ((encoded
          (encode (make-instance 'person :fname "Ada" :lname "Lovelace"))))
    (setf (jsown:val encoded "data") '("not" "an" "object"))
    (signals codec-validation-error
      (decode-document encoded))))

(test document-empty-list-and-false-use-slot-types
  (let* ((message
           (make-instance 'message
                          :content "hello"
                          :is-reply nil
                          :media nil))
         (wire (injective-round-trip (encode message)))
         (data (jsown:val wire "data")))
    (is (eq :false (jsown:val data "is_reply")))
    (is (and (listp (jsown:val data "media"))
             (null (jsown:val data "media"))))))

(test couchdb-revision-policy-is-preserved
  (let ((document (make-instance 'document :rev "1-valid")))
    (is (string= "1-valid" (jsown:val (encode document) "_rev"))))
  (let ((document (make-instance 'document :rev "invalid")))
    (is (not (nth-value 1 (jsown/get (encode document) "_rev"))))))
