(in-package :starintel.archive)

(defparameter +custody-states+
  '(:stored-unverified :verified :failed-integrity))

(defparameter +verification-states+
  '(:pending :verified :failed))

(defstruct (backup-manifest
            (:constructor make-backup-manifest
                (&key artifact-id
                      source
                      policy-digest
                      content-digest
                      byte-length
                      (state :complete))))
  artifact-id
  source
  policy-digest
  content-digest
  byte-length
  state)

(defstruct (custody-receipt
            (:constructor make-custody-receipt
                (&key artifact-id
                      location
                      backend
                      object-key
                      state
                      content-digest
                      byte-length)))
  artifact-id
  location
  backend
  object-key
  state
  content-digest
  byte-length)

(defstruct (verification-receipt
            (:constructor make-verification-receipt
                (&key artifact-id kind state content-digest)))
  artifact-id
  kind
  state
  content-digest)

(defun same-manifest-content-p (manifest artifact-id content-digest byte-length)
  (and (string= (backup-manifest-artifact-id manifest) artifact-id)
       (string= (backup-manifest-content-digest manifest) content-digest)
       (= (backup-manifest-byte-length manifest) byte-length)))

(defun verified-custody-p (manifest receipt &key (minimum :verified-remote))
  (and (backup-manifest-p manifest)
       (custody-receipt-p receipt)
       (member (custody-receipt-state receipt) +custody-states+ :test #'eq)
       (eq :verified (custody-receipt-state receipt))
       (same-manifest-content-p
        manifest
        (custody-receipt-artifact-id receipt)
        (custody-receipt-content-digest receipt)
        (custody-receipt-byte-length receipt))
       (ecase minimum
         (:verified-any
          (member (custody-receipt-location receipt)
                  '(:local :remote)
                  :test #'eq))
         (:verified-remote
          (eq :remote (custody-receipt-location receipt))))))

(defun restore-verified-p (manifest receipt)
  (and receipt
       (verification-receipt-p receipt)
       (eq :restore (verification-receipt-kind receipt))
       (member (verification-receipt-state receipt)
               +verification-states+
               :test #'eq)
       (eq :verified (verification-receipt-state receipt))
       (string= (backup-manifest-artifact-id manifest)
                (verification-receipt-artifact-id receipt))
       (string= (backup-manifest-content-digest manifest)
                (verification-receipt-content-digest receipt))))

(defun retention-eligible-p (manifest policy custody-receipts restore-receipt)
  "Return true only when destructive source retention is explicitly safe.

Upload success is deliberately insufficient. The policy must opt into deletion,
must not be pinned, the manifest must be complete and bound to the same policy
digest, required custody must be verified, and an explicit restore verification
receipt must match the archived content."
  (and (backup-manifest-p manifest)
       (archive-policy-p policy)
       (eq :complete (backup-manifest-state manifest))
       (archive-policy-delete-source-after-archive-p policy)
       (not (archive-policy-pinned-p policy))
       (string= (backup-manifest-policy-digest manifest)
                (policy-digest policy))
       (some (lambda (receipt)
               (verified-custody-p
                manifest
                receipt
                :minimum (archive-policy-minimum-custody policy)))
             custody-receipts)
       (restore-verified-p manifest restore-receipt)))