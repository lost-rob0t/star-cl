(in-package :starintel)

;;; Transitional v0.9 runtime binding for starintel-server#151.
;;;
;;; OPERATION is intentionally slotless: operation-specific structured state
;;; remains in DOCUMENT.DATA and is governed by the canonical StarIntel schema.
;;; Do not duplicate phase/dataset/capability semantics as Common Lisp slots.
;;; The final semantic authority is the canonical JSON-LD ontology system.

(defclass operation (document) ())
