#!/usr/bin/env -S sbcl --script
(require :asdf)
(let* ((root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-directory-pathname *load-truename*))))
  (push root asdf:*central-registry*)
  (let ((quicklisp (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp) (load quicklisp)))
  (let ((*standard-output* *error-output*))
    (asdf:load-system :starintel-0101)))

(handler-case
    (let* ((request (com.inuoe.jzon:parse *standard-input*))
           (command (gethash "command" request))
           (document (gethash "document" request))
           (response (starintel::json-object "ok" t)))
      (cond
        ((equal command "version")
         (setf (gethash "releaseVersion" response) (starintel.canonical:release-version)
               (gethash "schemaVersion" response) (starintel.canonical:schema-version)))
        ((equal command "capabilities")
         (setf (gethash "documentTypes" response) (coerce (starintel.canonical:document-types) 'vector)))
        ((equal command "validate") (starintel.canonical:validate-document document))
        ((equal command "roundtrip")
         (setf (gethash "document" response) (starintel.canonical:roundtrip-document document)))
        ((equal command "binding-roundtrip")
         (setf (gethash "document" response)
               (starintel.canonical:encode-document (starintel.canonical:decode-document document))))
        (t (error "Unsupported canonical command ~s" command)))
      (write-line (com.inuoe.jzon:stringify response)))
  (starintel::starintel-validation-error (condition)
    (write-line (com.inuoe.jzon:stringify
                 (starintel::json-object "ok" nil "error" (starintel::validation-category condition))))
    (uiop:quit 1)))
