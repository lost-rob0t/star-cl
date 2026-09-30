(asdf:defsystem :starintel-test
  :description "Required StarIntel 0.10.1 and legacy compatibility test suite"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.10.1"
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
                 (:file "operations-test")
                 (:file "json-test")
                 (:file "define-test")
                 (:file "v090-test")
                 (:file "v0101-test"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :starintel-test :run-tests)
               (error "StarIntel required tests failed."))))
