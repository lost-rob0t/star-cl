#!/usr/bin/env -S sbcl --script

(require :asdf)

(let ((quicklisp (merge-pathnames "quicklisp/setup.lisp"
                                  (user-homedir-pathname))))
  (when (probe-file quicklisp)
    (load quicklisp)))

(let* ((script (or *load-truename* *compile-file-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-parent-directory-pathname script))))
  (asdf:load-asd (merge-pathnames "starintel-v090.asd" root)))

(asdf:load-system :starintel-v090)

(in-package :cl-user)

(defun emit-json (value)
  (write-string (com.inuoe.jzon:stringify value) *standard-output*)
  (terpri *standard-output*)
  (finish-output *standard-output*))

(defun response-error (category message)
  (starintel::json-object
   "ok" nil
   "error" category
   "message" message))

(defun request-command (request)
  (starintel::hash-value request "command" ""))

(defun run-adapter ()
  (handler-case
      (let* ((request (com.inuoe.jzon:parse *standard-input*))
             (command (request-command request)))
        (unless (hash-table-p request)
          (emit-json (response-error "adapter_failure"
                                     "request must be a JSON object"))
          (return-from run-adapter 2))

        (when (string= command "version")
          (emit-json
           (starintel::json-object
            "ok" t
            "language" "cl"
            "spec_version" starintel::+starintel-schema-version+
            "adapter_version" starintel::+starintel-adapter-version+))
          (return-from run-adapter 0))

        (when (and (starintel::hash-present-p request "spec_version")
                   (not (string= (starintel::hash-value request "spec_version")
                                 starintel::+starintel-schema-version+)))
          (emit-json
           (response-error "unsupported_spec_version"
                           (princ-to-string
                            (starintel::hash-value request "spec_version"))))
          (return-from run-adapter 3))

        (let ((schema (starintel::load-v090-schema)))
          (cond
            ((string= command "capabilities")
             (let ((response (starintel::v090-capabilities schema)))
               (setf (gethash "ok" response) t)
               (emit-json response)
               0))
            ((string= command "schema-inventory")
             (emit-json
              (starintel::json-object
               "ok" t
               "spec_version" starintel::+starintel-schema-version+
               "inventory" (starintel::v090-schema-inventory schema)))
             0)
            ((not (starintel::hash-present-p request "document"))
             (emit-json (response-error "wrong_type" "document is required"))
             1)
            ((string= command "validate")
             (starintel::validate-v090-document
              (starintel::hash-value request "document") schema)
             (emit-json
              (starintel::json-object
               "ok" t
               "spec_version" starintel::+starintel-schema-version+
               "warnings" #()))
             0)
            ((or (string= command "normalize")
                 (string= command "roundtrip"))
             (let ((document
                     (starintel::roundtrip-v090-document
                      (starintel::hash-value request "document") schema)))
               (emit-json
                (starintel::json-object
                 "ok" t
                 "spec_version" starintel::+starintel-schema-version+
                 "document" document
                 "warnings" #())))
             0)
            (t
             (emit-json
              (response-error "adapter_failure"
                              (format nil "unsupported command: ~a" command)))
             2))))
    (starintel::starintel-validation-error (condition)
      (emit-json
       (response-error
        (starintel::validation-category condition)
        (starintel::validation-message condition)))
      (if (string= (starintel::validation-category condition)
                   "unsupported_spec_version")
          3
          1))
    (error (condition)
      (format *error-output* "common lisp adapter failure: ~a~%" condition)
      (emit-json (response-error "adapter_failure" (princ-to-string condition)))
      2)))

(uiop:quit (run-adapter))
