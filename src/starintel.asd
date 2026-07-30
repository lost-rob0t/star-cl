(asdf:defsystem :starintel
  :description "StarIntel v0.9.0 document parser, validator, and serializer"
  :author "nsaspy"
  :license "AGPL-3.0-only"
  :version "0.9.0"
  :serial t
  :depends-on (#:jsown #:com.inuoe.jzon #:ironclad #:local-time #:cms-ulid #:str #:cl-ppcre)
  :components ((:file "package")
               (:file "documents")
               (:file "entities")
               (:file "hosts")
               (:file "web")
               (:file "relations")
               (:file "targets")
               (:file "social-media")
               (:file "manifest")
               (:file "locations")
               (:file "v090")
               (:file "json")))
