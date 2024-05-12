(asdf:defsystem :starintel
  :description "Star Intel is a document Spec for handling osint data"
  :author "nsaspy"
  :license "LGLv3"
  :version "0.7.1"
  :serial t
  :depends-on (#:jsown #:ironclad #:local-time #:cms-ulid)
  :components ((:file "package")
               (:file "documents")
               (:file "entities")
               (:file "hosts")
               (:file "web")
               (:file "relations")
               (:file "targets")
               (:file "social-media")
               (:file "locations")
               (:file "define")))
