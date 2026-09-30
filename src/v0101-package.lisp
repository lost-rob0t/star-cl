(defpackage :starintel-v0101
  (:use :cl)
  (:export
   #:+adapter-version+
   #:+spec-version+
   #:capabilities
   #:json-object
   #:load-compatibility
   #:load-fixtures
   #:load-manifest
   #:load-schema
   #:migrate-batch
   #:migrate-document
   #:migration-error
   #:migration-reason-code
   #:roundtrip-document
   #:starintel-validation-error
   #:validate-document
   #:validation-category
   #:validation-message))
