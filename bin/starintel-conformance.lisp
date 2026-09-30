#!/usr/bin/env -S sbcl --script

(require :asdf)

(let ((*standard-output* *error-output*)
      (*trace-output* *error-output*)
      (*compile-verbose* nil)
      (*load-verbose* nil))
  (handler-case
      (asdf:load-system :starintel/v0101)
    (asdf:missing-component ()
      (let ((quicklisp (merge-pathnames "quicklisp/setup.lisp"
                                        (user-homedir-pathname))))
        (when (probe-file quicklisp)
          (load quicklisp)))
      (let* ((script (or *load-truename* *compile-file-truename*))
             (bin-directory (uiop:pathname-directory-pathname script))
             (root (uiop:pathname-parent-directory-pathname bin-directory))
             (asd (merge-pathnames "src/starintel.asd" root)))
        (asdf:load-asd (truename asd))
        (asdf:load-system :starintel/v0101)))))

(in-package :cl-user)

(defun emit-json (value)
  (write-string (com.inuoe.jzon:stringify value) *standard-output*)
  (terpri *standard-output*)
  (finish-output *standard-output*))

(defun response-error (category message)
  (starintel-v0101:json-object
   "ok" nil
   "error" category
   "message" message))

(defun request-command (request)
  (or (gethash "command" request) ""))

(defun request-value (request key &optional default)
  (multiple-value-bind (value presentp) (gethash key request)
    (if presentp value default)))

(defun request-present-p (request key)
  (nth-value 1 (gethash key request)))

(defun accepted-request-version-p (version)
  (member version '("0.9.0" "0.10.1") :test #'string=))

(defun migrate-request-document (request)
  (let* ((result
           (starintel-v0101:migrate-batch
            (vector (request-value request "document"))))
         (quarantine (gethash "quarantine" result)))
    (when (plusp (length quarantine))
      (error 'starintel-v0101:migration-error
             :reason-code (gethash "reasonCode" (aref quarantine 0))
             :message "document could not be migrated and validated"))
    result))

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
            (starintel-v0101:json-object
            "ok" t
            "language" "cl"
            "spec_version" starintel-v0101:+spec-version+
            "adapter_version" starintel-v0101:+adapter-version+))
          (return-from run-adapter 0))

        (when (and (request-present-p request "spec_version")
                   (not (accepted-request-version-p
                         (request-value request "spec_version"))))
          (emit-json
           (response-error "unsupported_spec_version"
                           (princ-to-string
                            (request-value request "spec_version"))))
          (return-from run-adapter 3))

        (cond
          ((string= command "capabilities")
           (let ((response (starintel-v0101:capabilities)))
             (setf (gethash "ok" response) t)
             (emit-json response)
             0))
          ((string= command "schema-inventory")
           (let ((capabilities (starintel-v0101:capabilities)))
             (emit-json
              (starintel-v0101:json-object
               "ok" t
               "spec_version" starintel-v0101:+spec-version+
               "inventory" (gethash "objectTypes" capabilities)))
             0))
          ((not (request-present-p request "document"))
           (emit-json (response-error "wrong_type" "document is required"))
           1)
          ((member command '("validate" "normalize" "roundtrip") :test #'string=)
           (let* ((result (migrate-request-document request))
                  (documents (gethash "documents" result))
                  (response
                    (starintel-v0101:json-object
                     "ok" t
                     "spec_version" starintel-v0101:+spec-version+
                     "warnings" #())))
             (unless (string= command "validate")
               (setf (gethash "document" response) (aref documents 0)
                     (gethash "documents" response) documents))
             (emit-json response)
             0))
          (t
           (emit-json
            (response-error "adapter_failure"
                            (format nil "unsupported command: ~a" command)))
           2)))
    (starintel-v0101:starintel-validation-error (condition)
      (emit-json
       (response-error
        (starintel-v0101:validation-category condition)
        (starintel-v0101:validation-message condition)))
      (if (string= (starintel-v0101:validation-category condition)
                   "unsupported_spec_version")
          3
          1))
    (starintel-v0101:migration-error (condition)
      (emit-json
       (response-error
        (starintel-v0101:migration-reason-code condition)
        (princ-to-string condition)))
      1)
    (error (condition)
      (format *error-output* "common lisp adapter failure: ~a~%" condition)
      (emit-json (response-error "adapter_failure" (princ-to-string condition)))
      2)))

(uiop:quit (run-adapter))
