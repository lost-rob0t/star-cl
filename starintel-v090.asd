(asdf:defsystem :starintel-v090
  :description "Standalone StarIntel v0.9.0 JSON validator and serializer"
  :author "nsaspy"
  :license "LGLv3"
  :version "0.9.0"
  :serial t
  :depends-on (#:com.inuoe.jzon #:cl-ppcre)
  :components ((:file "src/v090-package")
               (:file "src/v090")))
