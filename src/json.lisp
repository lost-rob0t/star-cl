(in-package :starintel)



(defun format-key (key)
  (if (str:starts-with? "_" key)
      (string-downcase key)
      (str:camel-case key)))


(defun as-json (object &key (format-fn #'format-key))
  (let ((json-obj (jsown:empty-object)))
    (loop for slot in (mapcar #'closer-mop:slot-definition-name
                              (closer-mop:class-slots (class-of object)))
          for value = (slot-value object slot)
          do (setf (jsown:val json-obj (funcall format-fn (string slot)))
                   (typecase value
                     (string value)
                     (integer value)
                     (list (jsown:to-json value))
                     (t (to-json value)))))
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

(defun from-json (json-obj class-name &key (format-fn #'format-key))
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for slot in (sb-mop:class-slots class)
          for slot-name = (sb-mop:slot-definition-name slot)
          for slot-type = (sb-mop:slot-definition-type slot)
          for key = (funcall format-fn (string slot-name))
          for value = (jsown:val-safe json-obj key)
          when value
            do (setf (slot-value object slot-name)
                     (cond
                       ((eq slot-type 'list) value)
                       ((eq slot-type 'string) value)
                       ((eq slot-type 'integer) value)
                       (t (from-json value (eval slot-type))))))
    object))




