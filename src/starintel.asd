(asdf:defsystem :starintel
  :description "Canonical Common Lisp runtime for the StarIntel 0.9.1 release on the v0.9 wire contract"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.9.1"
  :serial t
  :depends-on (#:jsown #:com.inuoe.jzon #:ironclad #:local-time #:cms-ulid #:str #:cl-ppcre #:closer-mop)
  :components ((:file "package")
               (:file "exports-v09")
               (:file "schema-org")
               (:file "types")
               (:file "documents")
               (:file "operations")
               (:file "entities")
               (:file "hosts")
               (:file "web")
               (:file "relations")
               (:file "targets")
               (:file "social-media")
               (:file "manifest")
               (:file "locations")
               (:file "v090")
               (:file "define")
               (:file "json")
               (:file "json-v09")))