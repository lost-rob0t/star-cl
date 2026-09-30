(asdf:defsystem :starintel/v0101
  :description "StarIntel 0.10.1 Star-Lang generated bindings, validation, and legacy migration"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.10.1"
  :pathname "../"
  :serial t
  :depends-on (#:babel #:com.inuoe.jzon #:ironclad #:cl-ppcre)
  :components ((:static-file "schema/starintel-0.10.1.schema.json")
               (:static-file "schema/starintel-0.10.1.manifest.json")
               (:static-file "schema/starintel-0.10.1.compatibility.json")
               (:static-file "schema/starintel-0.10.1.compatibility-fixtures.json")
               (:static-file "schema/starintel-0.10.1.release-lock.json")
               (:file "src/v0101-package")
               (:file "generated/starintel")
               (:file "src/v0101")
               (:file "src/migration-v0101")))

(asdf:defsystem :starintel
  :description "Common Lisp runtime for StarIntel 0.10.1 with explicit v0.9 compatibility"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.10.1"
  :serial t
  :depends-on (#:starintel/v0101 #:jsown #:com.inuoe.jzon #:ironclad #:local-time #:cms-ulid #:str #:cl-ppcre #:closer-mop)
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
