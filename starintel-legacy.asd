(asdf:defsystem :starintel-legacy
  :description "Explicit historical CLOS constructors and codecs"
  :author "nsaspy"
  :license "GPL-3.0-or-later"
  :version "0.9.1"
  :pathname "src/"
  :serial t
  :depends-on (#:starintel-v090 #:jsown #:com.inuoe.jzon #:ironclad #:local-time #:cms-ulid #:str #:cl-ppcre #:closer-mop)
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
               (:file "define")
               (:file "json")
               (:file "json-v09")))
