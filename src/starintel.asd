(asdf:defsystem :starintel
  :description "Canonical Common Lisp runtime for StarIntel document schema v0.9.0"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.9.0"
  :serial t
  :depends-on (#:jsown #:ironclad #:local-time #:cms-ulid #:str)
  :components ((:file "package")
               (:file "schema-org")
               (:file "documents")
               (:file "entities")
               (:file "hosts")
               (:file "web")
               (:file "relations")
               (:file "targets")
               (:file "social-media")
               (:file "manifest")
               (:file "locations")
               (:file "json")
               (:file "json-v09")))
