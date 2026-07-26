(in-package :starintel)

(define-condition codec-validation-error (error)
  ((operation
    :initarg :operation
    :reader codec-validation-operation)
   (path
    :initarg :path
    :initform nil
    :reader codec-validation-path)
   (expected-type
    :initarg :expected-type
    :reader codec-validation-expected-type)
   (value
    :initarg :value
    :reader codec-validation-value)
   (reason
    :initarg :reason
    :reader codec-validation-reason))
  (:report
   (lambda (condition stream)
     (format stream
             "StarIntel codec ~a error at ~{~a~^.~}: expected ~s; ~a; got ~s"
             (codec-validation-operation condition)
             (or (codec-validation-path condition) '("<root>"))
             (codec-validation-expected-type condition)
             (codec-validation-reason condition)
             (codec-validation-value condition)))))

(define-condition unknown-document-dtype (codec-validation-error) ())
(define-condition document-class-mismatch (codec-validation-error) ())

(defun codec-error (operation path expected value reason
                    &optional (condition-type 'codec-validation-error))
  (error condition-type
         :operation operation
         :path path
         :expected-type expected
         :value value
         :reason reason))

(defun format-key (key)
  (assert (stringp key) (key)
          "format-key: KEY must be a string, got: ~s" key)
  (if (str:starts-with? "_" key)
      (string-downcase key)
      (str:camel-case key)))

(defun camel-case-to-lisp-case (string)
  (assert (stringp string) (string)
          "camel-case-to-lisp-case: STRING must be a string, got: ~s" string)
  (with-output-to-string (stream)
    (loop for character across string
          for index from 0
          do (cond
               ((and (not (zerop index))
                     (upper-case-p character))
                (write-char #\- stream)
                (write-char (char-downcase character) stream))
               (t
                (write-char (char-downcase character) stream))))))

(defun jsown/get (object key)
  "Return VALUE and PRESENTP without conflating false, null, and absence."
  (assert object (object) "jsown/get: OBJECT is NIL")
  (assert (stringp key) (key)
          "jsown/get: KEY must be a string, got: ~s" key)
  (handler-case
      (values (jsown:val object key) t)
    (error ()
      (values nil nil))))

(defun json-object-p (value)
  (and (consp value)
       (eq (first value) :obj)))

(defun nullable-type-p (type-spec)
  (and (consp type-spec)
       (eq (first type-spec) 'or)
       (member 'null (rest type-spec) :test #'eq)))

(defun non-null-type-alternatives (type-spec)
  (if (and (consp type-spec)
           (eq (first type-spec) 'or))
      (remove 'null (rest type-spec) :test #'eq)
      (list type-spec)))

(defun normalized-atomic-type (type-spec)
  (if (symbolp type-spec)
      (let ((name (string-upcase (symbol-name type-spec))))
        (cond
          ((member name '("BOOL" "BOOLEAN") :test #'string=) 'boolean)
          ((member name '("INT" "INTEGER") :test #'string=) 'integer)
          ((member name '("FLOAT64" "DOUBLE-FLOAT") :test #'string=)
           'double-float)
          ((member name '("FLOAT" "SINGLE-FLOAT") :test #'string=)
           'single-float)
          ((string= name "NUMBER") 'number)
          ((string= name "STRING") 'string)
          ((string= name "LIST") 'list)
          ((string= name "VECTOR") 'vector)
          ((string= name "T") 't)
          (t type-spec)))
      type-spec))

(defun collection-type-p (type-spec kind)
  (or (eq (normalized-atomic-type type-spec) kind)
      (and (consp type-spec)
           (eq (normalized-atomic-type (first type-spec)) kind))))

(defun collection-element-type (type-spec)
  (if (and (consp type-spec)
           (rest type-spec))
      (second type-spec)
      't))

(defun user-class-symbol-p (symbol)
  "True only for user-defined STANDARD-CLASS objects."
  (when (symbolp symbol)
    (let ((class (find-class symbol nil)))
      (and class
           (not (eq (symbol-package symbol)
                    (find-package "COMMON-LISP")))
           (typep class 'standard-class)
           t))))

(defun valid-couchdb-revision-p (value)
  "Return true when VALUE looks like a CouchDB revision string."
  (and (stringp value)
       (> (length value) 2)
       (let ((dash (position #\- value)))
         (and dash
              (> dash 0)
              (< dash (1- (length value)))
              (every #'digit-char-p (subseq value 0 dash))))))

(defun slot-default-value (type-spec &key path)
  "Return the explicit unbound-slot default permitted by TYPE-SPEC."
  (let* ((alternatives (non-null-type-alternatives type-spec))
         (type (normalized-atomic-type (first alternatives))))
    (cond
      ((nullable-type-p type-spec) :null)
      ((eq type 'boolean) :false)
      ((collection-type-p type-spec 'list) #())
      ((collection-type-p type-spec 'vector) #())
      ((eq type 't) :null)
      (t
       (codec-error :encode path type-spec :unbound
                    "slot is unbound and has no type-defined default")))))

(defun slot-effective-value (object slot-name slot-type &key path)
  "Return a bound slot value or an explicit type-defined unbound default."
  (assert (typep object 'standard-object) (object)
          "slot-effective-value: OBJECT must be standard-object, got: ~s"
          object)
  (assert (symbolp slot-name) (slot-name)
          "slot-effective-value: SLOT-NAME must be symbol, got: ~s"
          slot-name)
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      (slot-default-value slot-type :path path)))

(defun encode-any-json-value (value format-fn path)
  (cond
    ((member value '(:true :false :null) :test #'eq) value)
    ((eq value t) :true)
    ((null value) :null)
    ((or (stringp value) (numberp value)) value)
    ((json-object-p value) value)
    ((typep value 'standard-object)
     (encode value :format-fn format-fn))
    ((listp value)
     (if value
         (loop for item in value
               for index from 0
               collect (encode-any-json-value
                        item format-fn
                        (append path (list index))))
         #()))
    ((vectorp value)
     (map 'vector
          (lambda (item)
            (encode-any-json-value item format-fn path))
          value))
    (t
     (codec-error :encode path 'json-value value
                  "value is not representable in JSON"))))

(defun encode-atomic-value (value type-spec format-fn path)
  (let ((type (normalized-atomic-type type-spec)))
    (cond
      ((eq type 't)
       (encode-any-json-value value format-fn path))
      ((eq type 'string)
       (if (stringp value)
           value
           (codec-error :encode path type-spec value
                        "value is not a string")))
      ((eq type 'integer)
       (if (integerp value)
           value
           (codec-error :encode path type-spec value
                        "value is not an integer")))
      ((member type '(single-float double-float) :test #'eq)
       (if (realp value)
           (coerce value type)
           (codec-error :encode path type-spec value
                        "value is not a real number")))
      ((eq type 'number)
       (if (numberp value)
           value
           (codec-error :encode path type-spec value
                        "value is not a number")))
      ((eq type 'boolean)
       (cond
         ((member value '(t :true) :test #'eq) :true)
         ((member value '(nil :false) :test #'eq) :false)
         (t
          (codec-error :encode path type-spec value
                       "value is not a JSON boolean"))))
      ((user-class-symbol-p type)
       (if (typep value type)
           (encode value :format-fn format-fn)
           (codec-error :encode path type-spec value
                        "value is not an instance of the declared class")))
      (t
       (handler-case
           (if (typep value type)
               value
               (codec-error :encode path type-spec value
                            "value does not satisfy the declared type"))
         (error ()
           (codec-error :encode path type-spec value
                        "unsupported slot type")))))))

(defun encode-collection-value (value type-spec format-fn path kind)
  (let ((element-type (collection-element-type type-spec)))
    (unless (if (eq kind 'list)
                (listp value)
                (vectorp value))
      (codec-error :encode path type-spec value
                   (format nil "value is not a ~a" kind)))
    (if (zerop (length value))
        #()
        (let ((encoded
                (loop for item across (coerce value 'vector)
                      for index from 0
                      collect
                      (encode-value item element-type
                                    :format-fn format-fn
                                    :path (append path (list index))))))
          (if (eq kind 'vector)
              (coerce encoded 'vector)
              encoded)))))

(defun try-encode-union (value alternatives format-fn path original-type)
  (dolist (alternative alternatives)
    (handler-case
        (return-from try-encode-union
          (encode-value value alternative
                        :format-fn format-fn
                        :path path))
      (codec-validation-error () nil)))
  (codec-error :encode path original-type value
               "value matches none of the union alternatives"))

(defun encode-value (value type-spec
                     &key (format-fn #'format-key) (path nil))
  "Encode VALUE according to TYPE-SPEC without collapsing JSON states."
  (cond
    ((eq value :null)
     (if (or (nullable-type-p type-spec)
             (eq (normalized-atomic-type type-spec) 't))
         :null
         (codec-error :encode path type-spec value
                      "JSON null is not allowed by this type")))
    ((and (null value) (nullable-type-p type-spec))
     :null)
    ((and (consp type-spec)
          (eq (first type-spec) 'or)
          (> (length (non-null-type-alternatives type-spec)) 1))
     (try-encode-union
      value
      (non-null-type-alternatives type-spec)
      format-fn
      path
      type-spec))
    ((collection-type-p type-spec 'list)
     (encode-collection-value value type-spec format-fn path 'list))
    ((collection-type-p type-spec 'vector)
     (encode-collection-value value type-spec format-fn path 'vector))
    (t
     (encode-atomic-value
      value
      (first (non-null-type-alternatives type-spec))
      format-fn
      path))))

(defun encode-standard-object (object &key (format-fn #'format-key))
  (let ((json-object (jsown:empty-object))
        (class (class-of object)))
    (closer-mop:finalize-inheritance class)
    (loop for slot-definition in (closer-mop:class-slots class)
          for slot-name = (closer-mop:slot-definition-name slot-definition)
          for slot-type = (closer-mop:slot-definition-type slot-definition)
          for slot-string = (string slot-name)
          for key = (funcall format-fn slot-string)
          for path = (list key)
          do
             (if (string= slot-string "_REV")
                 (when (and (slot-boundp object slot-name)
                            (valid-couchdb-revision-p
                             (slot-value object slot-name)))
                   (setf (jsown:val json-object key)
                         (encode-value
                          (slot-value object slot-name)
                          slot-type
                          :format-fn format-fn
                          :path path)))
                 (setf (jsown:val json-object key)
                       (encode-value
                        (slot-effective-value
                         object slot-name slot-type :path path)
                        slot-type
                        :format-fn format-fn
                        :path path))))
    json-object))

(defun encode (object &key (format-fn #'format-key))
  (assert (typep object 'standard-object) (object)
          "encode: OBJECT must be a standard-object, got: ~s" object)
  (encode-standard-object object :format-fn format-fn))

(defun json-array-p-for-type (value)
  (and (or (listp value) (vectorp value))
       (not (json-object-p value))))

(defun decode-any-json-value (value)
  value)

(defun decode-atomic-value (value type-spec format-fn path)
  (let ((type (normalized-atomic-type type-spec)))
    (cond
      ((eq type 't)
       (decode-any-json-value value))
      ((eq type 'string)
       (if (stringp value)
           value
           (codec-error :decode path type-spec value
                        "JSON value is not a string")))
      ((eq type 'integer)
       (if (integerp value)
           value
           (codec-error :decode path type-spec value
                        "JSON value is not an integer")))
      ((member type '(single-float double-float) :test #'eq)
       (if (realp value)
           (coerce value type)
           (codec-error :decode path type-spec value
                        "JSON value is not a real number")))
      ((eq type 'number)
       (if (numberp value)
           value
           (codec-error :decode path type-spec value
                        "JSON value is not a number")))
      ((eq type 'boolean)
       (cond
         ((member value '(t :true) :test #'eq) t)
         ((member value '(nil :false) :test #'eq) nil)
         (t
          (codec-error :decode path type-spec value
                       "JSON value is not a boolean"))))
      ((user-class-symbol-p type)
       (if (json-object-p value)
           (decode-standard-object value type :format-fn format-fn)
           (codec-error :decode path type-spec value
                        "JSON value is not an object")))
      (t
       (handler-case
           (if (typep value type)
               value
               (codec-error :decode path type-spec value
                            "JSON value does not satisfy the declared type"))
         (error ()
           (codec-error :decode path type-spec value
                        "unsupported slot type")))))))

(defun decode-collection-value (value type-spec format-fn path kind)
  (unless (json-array-p-for-type value)
    (codec-error :decode path type-spec value
                 "JSON value is not an array"))
  (let* ((element-type (collection-element-type type-spec))
         (decoded
           (loop for item across (coerce value 'vector)
                 for index from 0
                 collect
                 (decode-value item element-type
                               :format-fn format-fn
                               :path (append path (list index))))))
    (if (eq kind 'vector)
        (coerce decoded 'vector)
        decoded)))

(defun try-decode-union (value alternatives format-fn path original-type)
  (dolist (alternative alternatives)
    (handler-case
        (return-from try-decode-union
          (decode-value value alternative
                        :format-fn format-fn
                        :path path))
      (codec-validation-error () nil)))
  (codec-error :decode path original-type value
               "JSON value matches none of the union alternatives"))

(defun decode-value (value type-spec
                     &key (format-fn #'format-key) (path nil))
  "Decode VALUE according to TYPE-SPEC with strict JSON validation."
  (cond
    ((eq value :null)
     (cond
       ((nullable-type-p type-spec) nil)
       ((eq (normalized-atomic-type type-spec) 't) :null)
       (t
        (codec-error :decode path type-spec value
                     "JSON null is not allowed by this type"))))
    ((and (consp type-spec)
          (eq (first type-spec) 'or)
          (> (length (non-null-type-alternatives type-spec)) 1))
     (try-decode-union
      value
      (non-null-type-alternatives type-spec)
      format-fn
      path
      type-spec))
    ((collection-type-p type-spec 'list)
     (decode-collection-value value type-spec format-fn path 'list))
    ((collection-type-p type-spec 'vector)
     (decode-collection-value value type-spec format-fn path 'vector))
    (t
     (decode-atomic-value
      value
      (first (non-null-type-alternatives type-spec))
      format-fn
      path))))

(defun decode-standard-object (json-object class-name
                              &key (format-fn #'format-key))
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (closer-mop:finalize-inheritance class)
    (loop for slot-definition in (closer-mop:class-slots class)
          for slot-name = (closer-mop:slot-definition-name slot-definition)
          for slot-type = (closer-mop:slot-definition-type slot-definition)
          for key = (funcall format-fn (string slot-name))
          do
             (multiple-value-bind (value presentp)
                 (jsown/get json-object key)
               (when presentp
                 (setf (slot-value object slot-name)
                       (decode-value value slot-type
                                     :format-fn format-fn
                                     :path (list key))))))
    object))

(defun decode (json-object class-name &key (format-fn #'format-key))
  (assert json-object (json-object) "decode: JSON-OBJECT is NIL")
  (assert (symbolp class-name) (class-name)
          "decode: CLASS-NAME must be a symbol, got: ~s" class-name)
  (assert (not (eq class-name 't)) (class-name)
          "decode: refusing to decode into class T")
  (decode-standard-object json-object class-name :format-fn format-fn))
