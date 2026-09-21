(asdf:defsystem :starintel-archive
  :description "Core StarIntel backup/archive policy, custody, and backend protocol"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :serial t
  :depends-on (#:starintel)
  :components ((:module "src/archive"
                :components ((:file "package")
                             (:file "policy")
                             (:file "records")
                             (:file "protocol"))))
  :in-order-to ((test-op (test-op "starintel-archive-test"))))