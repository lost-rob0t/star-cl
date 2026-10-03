(defpackage :starintel.canonical
  (:use :cl)
  (:export #:release-version #:schema-version #:schema-path #:load-schema
           #:document-types #:validate-document #:roundtrip-document
           #:encode-document #:decode-document))
