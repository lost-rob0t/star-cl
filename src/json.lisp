(in-package :starintel)



(defun format-key (key)
  (if (str:starts-with? "_" key)
      (string-downcase key)
      (str:camel-case key)))


(defun encode (object &key (format-fn #'format-key))
  (let ((json-obj (jsown:empty-object)))
    (loop for slot in (mapcar #'closer-mop:slot-definition-name
                              (closer-mop:class-slots (class-of object)))
          for value = (slot-value object slot)
          do (setf (jsown:val json-obj (funcall format-fn (string slot)))
                   (typecase value
                     (string value)
                     (integer value)
                     (list (mapcar (lambda (item)
                                     (if (typep item 'standard-object)
                                         (encode item :format-fn format-fn)
                                         item))
                                   value))
                     (t (encode value :format-fn format-fn)))))
    json-obj))

(defun camel-case-to-lisp-case (string)
  (with-output-to-string (s)
    (loop for char across string
          for i from 0
          do (cond
               ((and (not (zerop i))
                     (upper-case-p char))
                (write-char #\- s)
                (write-char (char-downcase char) s))
               (t (write-char (char-downcase char) s))))))



;; Helper function to normalize slot types (unwrap OR types like (OR NULL TYPE))
(defun normalize-slot-type (type-spec)
  "Extract the actual type from wrapped type specs like (OR NULL TYPE)"
  (cond
    ((symbolp type-spec) type-spec)
    ((and (consp type-spec)
          (eq (first type-spec) 'or))
     ;; For (OR NULL TYPE), extract TYPE
     (find-if-not (lambda (t-spec) (eq t-spec 'null)) (rest type-spec)))
    ((consp type-spec) type-spec)
    (t type-spec)))

;; Helper function to check if a symbol names a class
(defun class-symbol-p (symbol)
  "Check if SYMBOL names a CLOS class"
  (and (symbolp symbol)
       (not (null symbol))
       (find-class symbol nil)))

;; Helper function to decode a value based on its type
(defun decode-value (value type-spec)
  "Decode VALUE according to TYPE-SPEC without using eval"
  (let ((normalized-type (normalize-slot-type type-spec)))
    (cond
      ;; Handle null/nil
      ((eq value :null) nil)
      ((null value) nil)

      ;; Handle list types: (LIST element-type)
      ((and (consp normalized-type)
            (eq (first normalized-type) 'list))
       (let ((element-type (second normalized-type)))
         (if (and element-type (class-symbol-p element-type))
             ;; List of objects
             (mapcar (lambda (item) (from-json item element-type))
                     value)
             ;; List of primitives
             value)))

      ;; Handle vector types: (VECTOR element-type)
      ((and (consp normalized-type)
            (eq (first normalized-type) 'vector))
       (let ((element-type (second normalized-type)))
         (if (and element-type (class-symbol-p element-type))
             ;; Vector of objects
             (map 'vector (lambda (item) (from-json item element-type))
                  value)
             ;; Vector of primitives
             (coerce value 'vector))))

      ;; Handle simple list type
      ((eq normalized-type 'list) value)

      ;; Handle primitive types
      ((eq normalized-type 'string) value)
      ((eq normalized-type 'integer) value)
      ((eq normalized-type 'float) (coerce value 'float))
      ((eq normalized-type 'number) value)
      ((eq normalized-type 'boolean) value)

      ;; Handle class types
      ((class-symbol-p normalized-type)
       (decode value normalized-type))

      ;; Default: return as-is
      (t value))))

(defun decode (json-obj class-name &key (format-fn #'format-key))
  "Decode JSON-OBJ into an instance of CLASS-NAME without using eval"
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for slot in (sb-mop:class-slots class)
          for slot-name = (sb-mop:slot-definition-name slot)
          for slot-type = (sb-mop:slot-definition-type slot)
          for key = (funcall format-fn (string slot-name))
          for value = (jsown:val-safe json-obj key)
          when value
            do (setf (slot-value object slot-name)
                     (decode-value value slot-type)))
    object))




