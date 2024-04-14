(asdf:defsystem :starintel
  :description "Star Intel is a document Spec for handling osint data"
  :author "nsaspy"
  :license "LGLv3"
  :version "0.1.0"
  :serial t
  :depends-on (#:jsown #:ironclad #:local-time #:uuid)
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
