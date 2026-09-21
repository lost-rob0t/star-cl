(asdf:defsystem :starintel-archive-test
  :description "Tests for the StarIntel archive policy/custody core"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on (#:starintel-archive #:fiveam)
  :serial t
  :components ((:module "t/archive"
                :components ((:file "package")
                             (:file "policy-test"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :starintel-archive-test :run-tests)
               (error "StarIntel archive tests failed."))))