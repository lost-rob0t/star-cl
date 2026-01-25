(in-package :starintel)

(defun format-key (key)
  (assert (stringp key) (key) "format-key: KEY must be a string, got: ~s" key)
  (if (str:starts-with? "_" key)
      (string-downcase key)
      (str:camel-case key)))

(defun camel-case-to-lisp-case (string)
  (assert (stringp string) (string) "camel-case-to-lisp-case: STRING must be a string, got: ~s" string)
  (with-output-to-string (s)
    (loop for ch across string
          for i from 0
          do (cond
               ((and (not (zerop i)) (upper-case-p ch))
                (write-char #\- s)
                (write-char (char-downcase ch) s))
               (t
                (write-char (char-downcase ch) s))))))

(defun normalize-slot-type (type-spec)
  "Extract the actual type from wrapped specs like (OR NULL TYPE)."
  (cond
    ((symbolp type-spec) type-spec)
    ((and (consp type-spec) (eq (first type-spec) 'or))
     (find-if-not (lambda (x) (eq x 'null)) (rest type-spec)))
    (t type-spec)))

(defun user-class-symbol-p (sym)
  "True only for user-defined STANDARD-CLASS, not CL built-ins like T/STRING/etc."
  (when (symbolp sym)
    (let ((cls (find-class sym nil)))
      (and cls
           (not (eq (symbol-package sym) (find-package "COMMON-LISP")))
           (typep cls 'standard-class)
           t))))

(defun jsown/get (obj key)
  "Return (values VALUE PRESENTP). PRESENTP is true iff KEY exists."
  (assert obj (obj) "jsown/get: OBJ is NIL")
  (assert (stringp key) (key) "jsown/get: KEY must be a string, got: ~s" key)
  (handler-case
      (values (jsown:val obj key) t)
    (error () (values nil nil))))

(defun slot-default-value (type-spec)
  "Default value used to ensure fields are always present in encoded JSON."
  (let ((t0 (normalize-slot-type type-spec)))
    (cond
      ((eq t0 'string) "")
      ((eq t0 'integer) 0)
      ((eq t0 'float) 0.0)
      ((eq t0 'number) 0)
      ((eq t0 'boolean) nil)              ; JSON false
      ((eq t0 'list) nil)                 ; JSON []
      ((and (consp t0) (eq (first t0) 'list)) nil)
      ((and (consp t0) (eq (first t0) 'vector)) (make-array 0))
      ((eq t0 't) :null)                  ; truly unknown -> JSON null
      ((user-class-symbol-p t0) nil)      ; object slot default = null unless initform bound
      (t :null))))

(defun slot-effective-value (object slot-name slot-type)
  "Return the slot value if bound/non-nil, else a type-based default.
Boolean NIL stays NIL (false)."
  (assert (typep object 'standard-object) (object)
          "slot-effective-value: OBJECT must be standard-object, got: ~s" object)
  (assert (symbolp slot-name) (slot-name)
          "slot-effective-value: SLOT-NAME must be symbol, got: ~s" slot-name)
  (let ((t0 (normalize-slot-type slot-type)))
    (cond
      ((slot-boundp object slot-name)
       (let ((v (slot-value object slot-name)))
         (cond
           ;; Keep boolean NIL as false (don't rewrite it).
           ((eq t0 'boolean) v)
           ;; Everything else: NIL means "use default" for spec-complete output.
           ((null v) (slot-default-value slot-type))
           (t v))))
      (t
       (slot-default-value slot-type)))))


(defun encode-value (value type-spec &key (format-fn #'format-key))
  (declare (ignore type-spec))
  (cond
    ((eq value :null) :null)
    ((stringp value) value)
    ((integerp value) value)
    ((floatp value) value)
    ((numberp value) value)
    ;; JSON booleans: t / nil
    ((or (eq value t) (eq value nil)) value)
    ((typep value 'standard-object)
     (encode value :format-fn format-fn))
    ((listp value)
     (mapcar (lambda (item)
               (if (typep item 'standard-object)
                   (encode item :format-fn format-fn)
                   item))
             value))
    ((vectorp value)
     (map 'vector (lambda (item)
                    (if (typep item 'standard-object)
                        (encode item :format-fn format-fn)
                        item))
          value))
    (t value)))

(defun encode (object &key (format-fn #'format-key))
  (assert (typep object 'standard-object) (object)
          "encode: OBJECT must be a standard-object, got: ~s" object)
  (let ((json-obj (jsown:empty-object)))
    (loop for sd in (closer-mop:class-slots (class-of object))
          for slot-name = (closer-mop:slot-definition-name sd)
          for slot-type = (closer-mop:slot-definition-type sd)
          for slot-str  = (string slot-name)
          for key       = (funcall format-fn slot-str)
          do
             ;; CouchDB: do NOT emit _rev unless it exists
             (if (string= slot-str "_REV")
                 (when (and (slot-boundp object slot-name)
                            (slot-value object slot-name))
                   (setf (jsown:val json-obj key)
                         (encode-value (slot-value object slot-name) slot-type
                                       :format-fn format-fn)))
                 ;; Everything else: always emit with defaults
                 (let ((v (slot-effective-value object slot-name slot-type)))
                   (setf (jsown:val json-obj key)
                         (encode-value v slot-type :format-fn format-fn)))))
    json-obj))


(defun decode-value (value type-spec &key (format-fn #'format-key))
  (let ((t0 (normalize-slot-type type-spec)))
    (cond
      ((eq value :null) nil)

      ;; T means "anything" — leave it alone.
      ((eq t0 't) value)

      ;; primitives
      ((eq t0 'string) value)
      ((eq t0 'integer) value)
      ((eq t0 'float) (if (null value) nil (coerce value 'float)))
      ((eq t0 'number) value)
      ((eq t0 'boolean) (and value t))

      ;; lists/vectors
      ((and (consp t0) (eq (first t0) 'list))
       (let ((elt (second t0)))
         (cond
           ((null value) nil)
           ((user-class-symbol-p elt)
            (mapcar (lambda (item) (decode item elt :format-fn format-fn)) value))
           (t value))))

      ((and (consp t0) (eq (first t0) 'vector))
       (let ((elt (second t0)))
         (cond
           ((null value) (make-array 0))
           ((user-class-symbol-p elt)
            (map 'vector (lambda (item) (decode item elt :format-fn format-fn)) value))
           (t (coerce value 'vector)))))

      ((eq t0 'list) value)

      ;; user objects
      ((user-class-symbol-p t0)
       (decode value t0 :format-fn format-fn))

      (t value))))

(defun decode (json-obj class-name &key (format-fn #'format-key))
  (assert json-obj (json-obj) "decode: JSON-OBJ is NIL")
  (assert (symbolp class-name) (class-name)
          "decode: CLASS-NAME must be a symbol, got: ~s" class-name)
  (assert (not (eq class-name 't)) (class-name)
          "decode: refusing to decode into class T (means 'anything')")

  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for sd in (sb-mop:class-slots class)
          for slot-name = (sb-mop:slot-definition-name sd)
          for slot-type = (sb-mop:slot-definition-type sd)
          for key = (funcall format-fn (string slot-name))
          do
             ;; If slot is somehow unbound even after MAKE-INSTANCE, force a default.
             (unless (slot-boundp object slot-name)
               (setf (slot-value object slot-name)
                     (slot-default-value slot-type)))

             ;; If JSON has the key, overwrite; if missing, keep initform/default.
             (multiple-value-bind (val presentp) (jsown/get json-obj key)
               (when presentp
                 (setf (slot-value object slot-name)
                       (decode-value val slot-type :format-fn format-fn)))))
    object))
