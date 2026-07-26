(asdf:defsystem :starintel-test
  :description "Test suite for StarIntel"
  :author "nsaspy"
  :license "LGLv3"
  :version "0.9.0"
  :depends-on (#:starintel #:fiveam)
  :serial t
  :components ((:module "t"
                :components
                ((:file "package")
                 (:file "documents-test")
                 (:file "entities-test")
                 (:file "hosts-test")
                 (:file "web-test")
                 (:file "relations-test")
                 (:file "json-test")
                 (:file "define-test")
                 (:file "v090-test"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (symbol-call :fiveam '#:run! :starintel-test)))
