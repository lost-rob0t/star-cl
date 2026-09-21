(in-package :starintel-archive-test)

(in-suite archive-test)

(test defaults-are-nondestructive
  (let ((policy (make-policy)))
    (is (= 3600 (archive-policy-snapshot-every-seconds policy)))
    (is (= 86400 (archive-policy-full-snapshot-every-seconds policy)))
    (is (= 14 (archive-policy-local-retention-days policy)))
    (is (= 365 (archive-policy-remote-retention-days policy)))
    (is (eq :verified-remote (archive-policy-minimum-custody policy)))
    (is (= 7 (archive-policy-restore-verify-every-runs policy)))
    (is (not (archive-policy-delete-source-after-archive-p policy)))))

(test operator-specificity-is-deterministic
  (let* ((base (make-policy))
         (dtype (make-policy-rule
                 "dtype-observation"
                 :authority :operator
                 :selector (make-policy-selector :dtype "observation")
                 :overrides '(:remote-retention-days 730)))
         (target (make-policy-rule
                  "target-case"
                  :authority :operator
                  :selector (make-policy-selector :target-id "target:42")
                  :overrides '(:remote-retention-days 2555 :pinned t)))
         (resolved (resolve-policy
                    base
                    (list target dtype)
                    (make-archive-context
                     :dtype "observation"
                     :target-id "target:42"))))
    (is (= 2555
           (archive-policy-remote-retention-days
            (resolved-policy-policy resolved))))
    (is (archive-policy-pinned-p (resolved-policy-policy resolved)))
    (is (equal '("dtype-observation" "target-case")
               (resolved-policy-applied-rule-ids resolved)))))

(test actor-policy-can-strengthen-but-not-weaken
  (let* ((base (make-policy :remote-retention-days 365))
         (rule (make-policy-rule
                "actor-request"
                :authority :actor
                :selector (make-policy-selector :actor-id "collector")
                :overrides '(:remote-retention-days 30
                             :delete-source-after-archive t
                             :pinned t)))
         (resolved (resolve-policy
                    base
                    (list rule)
                    (make-archive-context :actor-id "collector")))
         (policy (resolved-policy-policy resolved)))
    (is (= 365 (archive-policy-remote-retention-days policy)))
    (is (not (archive-policy-delete-source-after-archive-p policy)))
    (is (archive-policy-pinned-p policy))
    (is (member "actor-request"
                (resolved-policy-rejected-rule-ids resolved)
                :test #'string=))))

(test policy-digest-is-stable
  (let* ((context (make-archive-context :dtype "observation"))
         (rule-a (make-policy-rule
                  "a"
                  :selector (make-policy-selector :dtype "observation")
                  :overrides '(:remote-retention-days 730)))
         (rule-b (make-policy-rule
                  "b"
                  :selector (make-policy-selector :dtype "observation")
                  :overrides '(:local-retention-days 30)))
         (one (resolve-policy (make-policy) (list rule-b rule-a) context))
         (two (resolve-policy (make-policy) (list rule-a rule-b) context)))
    (is (string= (resolved-policy-digest one)
                 (resolved-policy-digest two)))))

(test eviction-needs-explicit-policy-custody-and-restore-proof
  (let* ((policy (make-policy :delete-source-after-archive t))
         (manifest (make-backup-manifest
                    :artifact-id "artifact"
                    :source "couchdb:starintel"
                    :policy-digest (policy-digest policy)
                    :content-digest "sha256:abc"
                    :byte-length 42))
         (local (make-custody-receipt
                 :artifact-id "artifact"
                 :location :local
                 :backend "local-s3"
                 :state :verified
                 :content-digest "sha256:abc"
                 :byte-length 42))
         (remote (make-custody-receipt
                  :artifact-id "artifact"
                  :location :remote
                  :backend "linode"
                  :state :verified
                  :content-digest "sha256:abc"
                  :byte-length 42))
         (restore (make-verification-receipt
                   :artifact-id "artifact"
                   :kind :restore
                   :state :verified
                   :content-digest "sha256:abc")))
    (is (not (retention-eligible-p manifest policy (list local) restore)))
    (is (retention-eligible-p manifest policy (list remote) restore))
    (is (not (retention-eligible-p manifest policy (list remote) nil)))))

(test pinned-policy-never-evicts
  (let* ((policy (make-policy :delete-source-after-archive t :pinned t))
         (manifest (make-backup-manifest
                    :artifact-id "artifact"
                    :source "couchdb:starintel"
                    :policy-digest (policy-digest policy)
                    :content-digest "sha256:abc"
                    :byte-length 42))
         (remote (make-custody-receipt
                  :artifact-id "artifact"
                  :location :remote
                  :backend "linode"
                  :state :verified
                  :content-digest "sha256:abc"
                  :byte-length 42))
         (restore (make-verification-receipt
                   :artifact-id "artifact"
                   :kind :restore
                   :state :verified
                   :content-digest "sha256:abc")))
    (is (not (retention-eligible-p manifest policy (list remote) restore)))))