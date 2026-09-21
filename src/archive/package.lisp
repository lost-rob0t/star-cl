(in-package :cl-user)

(uiop:define-package :starintel.archive
  (:use :cl)
  (:export
   ;; policy
   #:archive-policy
   #:make-policy
   #:archive-policy-snapshot-every-seconds
   #:archive-policy-full-snapshot-every-seconds
   #:archive-policy-local-retention-days
   #:archive-policy-remote-retention-days
   #:archive-policy-minimum-custody
   #:archive-policy-restore-verify-every-runs
   #:archive-policy-delete-source-after-archive-p
   #:archive-policy-pinned-p
   #:policy-digest
   #:policy-selector
   #:make-policy-selector
   #:policy-selector-tenant
   #:policy-selector-dataset
   #:policy-selector-dtype
   #:policy-selector-target-id
   #:policy-selector-actor-id
   #:archive-context
   #:make-archive-context
   #:archive-context-tenant
   #:archive-context-dataset
   #:archive-context-dtype
   #:archive-context-target-id
   #:archive-context-actor-id
   #:policy-rule
   #:make-policy-rule
   #:policy-rule-id
   #:policy-rule-authority
   #:policy-rule-selector
   #:policy-rule-overrides
   #:resolved-policy
   #:resolved-policy-policy
   #:resolved-policy-digest
   #:resolved-policy-applied-rule-ids
   #:resolved-policy-rejected-rule-ids
   #:resolve-policy
   #:archive-policy-error
   #:archive-policy-error-reason
   ;; records / custody
   #:backup-manifest
   #:make-backup-manifest
   #:backup-manifest-artifact-id
   #:backup-manifest-source
   #:backup-manifest-policy-digest
   #:backup-manifest-content-digest
   #:backup-manifest-byte-length
   #:backup-manifest-state
   #:custody-receipt
   #:make-custody-receipt
   #:custody-receipt-artifact-id
   #:custody-receipt-location
   #:custody-receipt-backend
   #:custody-receipt-object-key
   #:custody-receipt-state
   #:custody-receipt-content-digest
   #:custody-receipt-byte-length
   #:verification-receipt
   #:make-verification-receipt
   #:verification-receipt-artifact-id
   #:verification-receipt-kind
   #:verification-receipt-state
   #:verification-receipt-content-digest
   #:verified-custody-p
   #:retention-eligible-p
   ;; backend protocol
   #:archive-backend
   #:backend-capabilities
   #:backend-put-artifact
   #:backend-stat-artifact
   #:backend-read-artifact
   #:backend-delete-artifact))