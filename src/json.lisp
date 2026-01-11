(in-package :starintel)

(defun format-key (key)
  "Convert symbol name to Nim-style JSON key.
  Underscore-prefixed names stay lowercase, others become camelCase."
  (if (str:starts-with? "_" key)
      (string-downcase key)
      (str:camel-case key)))

(defun safe-slot-value (obj slot)
  "Return value of SLOT in OBJ or NIL if unbound."
  (when (and (slot-exists-p obj slot)
             (slot-boundp obj slot))
    (slot-value obj slot)))

(defun doc->jsown (doc &key (format-fn #'format-key))
  "Convert any STARINTEL document into a JSOWN :obj form,
   applying Nim-style key formatting."
  (cons :obj
        (loop for slot in (mapcar #'closer-mop:slot-definition-name
                                  (closer-mop:class-slots (class-of doc)))
              for val = (safe-slot-value doc slot)
              when val
                collect (cons (funcall format-fn (string slot)) val))))

(defun encode (doc &key (pretty nil))
  "Encode any STARINTEL object into JSON using JSOWN.
   Applies Nim-style key naming."
  (declare (ignore pretty))
  (jsown:to-json (doc->jsown doc)))

(defun decode (json-obj class-name &key (format-fn #'format-key))
  "Instantiate CLASS-NAME from a JSOWN object, reversing camelCase keys.
   Automatically converts values and sets only bound slots."
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for slot in (closer-mop:class-slots class)
          for slot-name = (closer-mop:slot-definition-name slot)
          for key = (funcall format-fn (string slot-name))
          for value = (jsown:val-safe json-obj key)
          when value
            do (setf (slot-value object slot-name) value))
    object))
