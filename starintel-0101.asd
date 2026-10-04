(asdf:defsystem :starintel-0101
  :description "StarLang-generated StarIntel contract and canonical JSON boundary"
  :version "0.10.1"
  :license "GPL-3.0-or-later"
  :depends-on (#:starintel-v090)
  :serial t
  :components ((:file "schemas/starintel-0.10.1/generated/starintel")
               (:file "src/v0101-package")
               (:file "src/operation-semantics")
               (:file "src/v0101")))
