(in-package :starintel.archive)

(defclass archive-backend () ())

(defgeneric backend-capabilities (backend)
  (:documentation "Return the closed capability set advertised by BACKEND."))

(defgeneric backend-put-artifact (backend artifact-id pathname metadata)
  (:documentation
   "Put an immutable artifact. Reusing ARTIFACT-ID with different bytes must fail."))

(defgeneric backend-stat-artifact (backend artifact-id)
  (:documentation "Return provider-neutral immutable object metadata or NIL."))

(defgeneric backend-read-artifact (backend artifact-id destination)
  (:documentation "Read ARTIFACT-ID into DESTINATION for verification/restore."))

(defgeneric backend-delete-artifact (backend artifact-id retention-decision)
  (:documentation
   "Delete only when RETENTION-DECISION is an explicit trusted authorization."))