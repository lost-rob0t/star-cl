(in-package :starintel)

(defun v09-wire-key (slot-name)
  (substitute #\_ #\-
              (string-downcase (string slot-name))))

(defun encode-source-v09 (source)
  (if (stringp source)
      (let ((object (jsown:empty-object)))
        (setf (jsown:val object "kind") "web"
              (jsown:val object "name") source
              (jsown:val object "uri") source
              (jsown:val object "url") source)
        object)
      source))

(defun encode-standard-object-v09 (object &key (format-fn #'format-key))
  (let ((json-obj (jsown:empty-object)))
    (loop for sd in (closer-mop:class-slots (class-of object))
          for slot-name = (closer-mop:slot-definition-name sd)
          for slot-type = (closer-mop:slot-definition-type sd)
          for slot-str = (string slot-name)
          for key = (funcall format-fn slot-str)
          do
             (if (string= slot-str "_REV")
                 (when (and (slot-boundp object slot-name)
                            (valid-couchdb-revision-p (slot-value object slot-name)))
                   (setf (jsown:val json-obj key)
                         (encode-value (slot-value object slot-name) slot-type
                                       :format-fn format-fn)))
                 (let ((value (slot-effective-value object slot-name slot-type)))
                   (setf (jsown:val json-obj key)
                         (encode-value value slot-type :format-fn format-fn)))))
    json-obj))

(defun encode-document-v09 (object)
  (refresh-schema-org object)
  (let ((json-obj (jsown:empty-object))
        (data (or (doc-data object) (jsown:empty-object))))
    (loop for sd in (closer-mop:class-slots (class-of object))
          for slot-name = (closer-mop:slot-definition-name sd)
          for slot-type = (closer-mop:slot-definition-type sd)
          for key = (v09-wire-key slot-name)
          unless (eq slot-name 'data)
            do
               (cond
                 ((eq slot-name '_rev)
                  (when (and (slot-boundp object slot-name)
                             (valid-couchdb-revision-p (slot-value object slot-name)))
                    (setf (jsown:val json-obj key)
                          (encode-value (slot-value object slot-name) slot-type))))
                 ((document-envelope-slot-p slot-name)
                  (let ((value (slot-effective-value object slot-name slot-type)))
                    (setf (jsown:val json-obj key)
                          (if (eq slot-name 'sources)
                              (mapcar #'encode-source-v09 value)
                              (encode-value value slot-type)))))
                 (t
                  (let ((value (slot-effective-value object slot-name slot-type)))
                    (setf (jsown:val data key)
                          (encode-value value slot-type))))))
    (setf (jsown:val json-obj "data") data)
    json-obj))

(defun encode (object &key (format-fn #'format-key))
  (assert (typep object 'standard-object) (object)
          "encode: OBJECT must be a standard-object, got: ~s" object)
  (if (typep object 'document)
      (encode-document-v09 object)
      (encode-standard-object-v09 object :format-fn format-fn)))

(defun decode-standard-object-v09 (json-obj class-name &key (format-fn #'format-key))
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for sd in (closer-mop:class-slots class)
          for slot-name = (closer-mop:slot-definition-name sd)
          for slot-type = (closer-mop:slot-definition-type sd)
          for key = (funcall format-fn (string slot-name))
          do
             (unless (slot-boundp object slot-name)
               (setf (slot-value object slot-name)
                     (slot-default-value slot-type)))
             (multiple-value-bind (value presentp) (jsown/get json-obj key)
               (when presentp
                 (setf (slot-value object slot-name)
                       (decode-value value slot-type :format-fn format-fn)))))
    object))

(defun decode-document-v09 (json-obj class-name)
  (let* ((object (make-instance class-name))
         (class (class-of object))
         (data (multiple-value-bind (value presentp) (jsown/get json-obj "data")
                 (if presentp value (jsown:empty-object)))))
    (loop for sd in (closer-mop:class-slots class)
          for slot-name = (closer-mop:slot-definition-name sd)
          for slot-type = (closer-mop:slot-definition-type sd)
          for key = (v09-wire-key slot-name)
          do
             (cond
               ((eq slot-name 'data)
                (setf (slot-value object slot-name) data))
               ((document-envelope-slot-p slot-name)
                (multiple-value-bind (value presentp) (jsown/get json-obj key)
                  (when presentp
                    (setf (slot-value object slot-name)
                          (decode-value value slot-type)))))
               (t
                (multiple-value-bind (value presentp) (jsown/get data key)
                  (when presentp
                    (setf (slot-value object slot-name)
                          (decode-value value slot-type)))))))
    (set-type object)
    (refresh-schema-org object)
    object))

(defun decode (json-obj class-name &key (format-fn #'format-key))
  (assert json-obj (json-obj) "decode: JSON-OBJ is NIL")
  (assert (symbolp class-name) (class-name)
          "decode: CLASS-NAME must be a symbol, got: ~s" class-name)
  (assert (not (eq class-name 't)) (class-name)
          "decode: refusing to decode into class T (means 'anything')")
  (if (nth-value 0 (subtypep class-name 'document))
      (decode-document-v09 json-obj class-name)
      (decode-standard-object-v09 json-obj class-name :format-fn format-fn)))
