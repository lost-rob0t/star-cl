(in-package :starintel)

(defparameter *document-class-registry*
  (make-hash-table :test #'equal))

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
      (encode-value source 't :path '("sources"))))

(defun json-value-v09 (object key &optional default)
  (multiple-value-bind (value presentp) (jsown/get object key)
    (if presentp value default)))

(defun set-json-default-v09 (object key value)
  (unless (nth-value 1 (jsown/get object key))
    (setf (jsown:val object key) value))
  object)

(defun normalize-required-data-v09 (dtype data)
  (cond
    ((string= dtype "relation")
     (set-json-default-v09
      data "subject" (json-value-v09 data "source" ""))
     (set-json-default-v09
      data "object" (json-value-v09 data "target" ""))
     (set-json-default-v09
      data "source" (json-value-v09 data "subject" ""))
     (set-json-default-v09
      data "target" (json-value-v09 data "object" "")))
    ((string= dtype "domain")
     (set-json-default-v09
      data "domain" (json-value-v09 data "record" "")))
    ((string= dtype "email")
     (let ((user (json-value-v09 data "user" ""))
           (domain (json-value-v09 data "domain" "")))
       (set-json-default-v09
        data
        "address"
        (if (and (stringp user)
                 (stringp domain)
                 (> (length user) 0)
                 (> (length domain) 0))
            (format nil "~a@~a" user domain)
            ""))))
    ((string= dtype "email-message")
     (let ((to (json-value-v09 data "to" nil))
           (headers (json-value-v09 data "headers" nil)))
       (when (stringp to)
         (setf (jsown:val data "to")
               (if (> (length to) 0)
                   (list to)
                   #())))
       (when (stringp headers)
         (let ((object (jsown:empty-object)))
           (when (> (length headers) 0)
             (setf (jsown:val object "raw") headers))
           (setf (jsown:val data "headers") object))))))
  data)

(defun document-class-object (class-designator)
  (cond
    ((symbolp class-designator)
     (or (find-class class-designator nil)
         (codec-error :decode '("dtype") 'document-class
                      class-designator "unknown class designator")))
    ((ignore-errors (class-name class-designator))
     class-designator)
    (t
     (codec-error :decode '("dtype") 'document-class
                  class-designator "unknown class designator"))))

(defun starintel-document-class-p (class)
  (let ((name (class-name class)))
    (and name
         (ignore-errors
           (nth-value 0 (subtypep name 'document))))))

(defun register-document-class (dtype class-designator)
  (let* ((class (document-class-object class-designator))
         (canonical (canonical-dtype dtype)))
    (unless (starintel-document-class-p class)
      (codec-error :decode '("dtype") 'document-class class-designator
                   "class is not a StarIntel document class"))
    (setf (gethash canonical *document-class-registry*) class)
    class))

(defun refresh-document-class-registry ()
  (clrhash *document-class-registry*)
  (let ((package (find-package :starintel)))
    (loop for symbol being the external-symbols of package
          for class = (find-class symbol nil)
          when (and class (starintel-document-class-p class))
            do
               (register-document-class
                (canonical-dtype
                 (string-downcase (symbol-name symbol)))
                class)))
  *document-class-registry*)

(defun registered-document-class (dtype &key (errorp t))
  (let* ((canonical (canonical-dtype dtype))
         (class (gethash canonical *document-class-registry*)))
    (cond
      (class class)
      (errorp
       (codec-error
        :decode
        '("dtype")
        'registered-document-dtype
        dtype
        "dtype has no registered document class"
        'unknown-document-dtype))
      (t nil))))

(defun registered-document-dtypes ()
  (sort
   (loop for dtype being the hash-keys of *document-class-registry*
         collect dtype)
   #'string<))

(defun document-dtype-v09 (json-object)
  (multiple-value-bind (dtype presentp)
      (jsown/get json-object "dtype")
    (unless (and presentp (stringp dtype))
      (codec-error :decode '("dtype") 'string dtype
                   "document dtype is required"))
    (canonical-dtype dtype)))

(defun validate-requested-document-class (json-object class-designator)
  (let* ((dtype (document-dtype-v09 json-object))
         (registered (registered-document-class dtype))
         (requested (document-class-object class-designator)))
    (unless (eq registered requested)
      (codec-error
       :decode
       '("dtype")
       (class-name registered)
       (class-name requested)
       (format nil "dtype ~a resolves to a different registered class" dtype)
       'document-class-mismatch))
    registered))

(defun encode-standard-object-v09 (object &key (format-fn #'format-key))
  (encode-standard-object object :format-fn format-fn))

(defun encode-document-v09 (object)
  (refresh-schema-org object)
  (let ((json-object (jsown:empty-object))
        (data (or (doc-data object) (jsown:empty-object)))
        (class (class-of object)))
    (unless (json-object-p data)
      (codec-error :encode '("data") 'json-object data
                   "document data must be a JSON object"))
    (closer-mop:finalize-inheritance class)
    (loop for slot-definition in (closer-mop:class-slots class)
          for slot-name = (closer-mop:slot-definition-name slot-definition)
          for slot-type = (closer-mop:slot-definition-type slot-definition)
          for key = (v09-wire-key slot-name)
          for path = (if (document-envelope-slot-p slot-name)
                         (list key)
                         (list "data" key))
          unless (eq slot-name 'data)
            do
               (cond
                 ((eq slot-name '_rev)
                  (when (and (slot-boundp object slot-name)
                             (valid-couchdb-revision-p
                              (slot-value object slot-name)))
                    (setf (jsown:val json-object key)
                          (encode-value
                           (slot-value object slot-name)
                           slot-type
                           :path path))))
                 ((document-envelope-slot-p slot-name)
                  (let ((value
                          (slot-effective-value
                           object slot-name slot-type :path path)))
                    (setf (jsown:val json-object key)
                          (if (eq slot-name 'sources)
                              (if (zerop (length value))
                                  #()
                                  (mapcar #'encode-source-v09 value))
                              (encode-value value slot-type :path path)))))
                 (t
                  (let ((value
                          (slot-effective-value
                           object slot-name slot-type :path path)))
                    (setf (jsown:val data key)
                          (encode-value value slot-type :path path))))))
    (normalize-required-data-v09 (doc-type object) data)
    (setf (jsown:val json-object "data") data)
    json-object))

(defun encode (object &key (format-fn #'format-key))
  "Return a JSOWN object. Call JSOWN:TO-JSON to serialize it."
  (assert (typep object 'standard-object) (object)
          "encode: OBJECT must be a standard-object, got: ~s" object)
  (if (typep object 'document)
      (encode-document-v09 object)
      (encode-standard-object-v09 object :format-fn format-fn)))

(defun decode-standard-object-v09
    (json-object class-name &key (format-fn #'format-key))
  (decode-standard-object
   json-object class-name :format-fn format-fn))

(defun decode-document-v09 (json-object class-name)
  (let* ((registered-class
           (validate-requested-document-class json-object class-name))
         (registered-name (class-name registered-class))
         (object (make-instance registered-name))
         (class (class-of object))
         (data
           (multiple-value-bind (value presentp)
               (jsown/get json-object "data")
             (if presentp
                 value
                 (jsown:empty-object)))))
    (unless (json-object-p data)
      (codec-error :decode '("data") 'json-object data
                   "document data must be a JSON object"))
    (closer-mop:finalize-inheritance class)
    (loop for slot-definition in (closer-mop:class-slots class)
          for slot-name = (closer-mop:slot-definition-name slot-definition)
          for slot-type = (closer-mop:slot-definition-type slot-definition)
          for key = (v09-wire-key slot-name)
          do
             (cond
               ((eq slot-name 'data)
                (setf (slot-value object slot-name) data))
               ((document-envelope-slot-p slot-name)
                (multiple-value-bind (value presentp)
                    (jsown/get json-object key)
                  (when presentp
                    (setf (slot-value object slot-name)
                          (decode-value
                           value slot-type :path (list key))))))
               (t
                (multiple-value-bind (value presentp)
                    (jsown/get data key)
                  (when presentp
                    (setf (slot-value object slot-name)
                          (decode-value
                           value
                           slot-type
                           :path (list "data" key))))))))
    (set-type object)
    (refresh-schema-org object)
    object))

(defun decode-document (json-object)
  "Decode a v0.9 document using its registered dtype class."
  (let ((class (registered-document-class
                (document-dtype-v09 json-object))))
    (decode-document-v09 json-object (class-name class))))

(defun decode (json-object class-name &key (format-fn #'format-key))
  (assert json-object (json-object) "decode: JSON-OBJECT is NIL")
  (assert (symbolp class-name) (class-name)
          "decode: CLASS-NAME must be a symbol, got: ~s" class-name)
  (assert (not (eq class-name 't)) (class-name)
          "decode: refusing to decode into class T")
  (let ((class (find-class class-name nil)))
    (unless class
      (codec-error :decode nil 'standard-class class-name
                   "class does not exist"))
    (if (starintel-document-class-p class)
        (decode-document-v09 json-object class-name)
        (decode-standard-object-v09
         json-object class-name :format-fn format-fn))))

(eval-when (:load-toplevel :execute)
  (refresh-document-class-registry))
