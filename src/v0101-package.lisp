(defpackage :starintel.canonical
  (:use :cl)
  (:import-from :starintel #:parse-json #:stringify-json #:json-number #:json-number-p #:json-number-lexeme)
  (:export #:parse-json #:stringify-json #:json-number #:json-number-p #:json-number-lexeme
           #:release-version #:schema-version #:schema-path #:load-schema
           #:document-types #:validate-document #:roundtrip-document
           #:encode-document #:decode-document))
