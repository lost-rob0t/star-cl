(in-package :starintel.archive)

(define-condition archive-policy-error (error)
  ((reason :initarg :reason :reader archive-policy-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "StarIntel archive policy error: ~A"
             (archive-policy-error-reason condition)))))

(defun policy-error (control &rest arguments)
  (error 'archive-policy-error
         :reason (apply #'format nil control arguments)))

(defparameter +minimum-custody-values+
  '(:verified-any :verified-remote))

(defparameter +override-keys+
  '(:snapshot-every-seconds
    :full-snapshot-every-seconds
    :local-retention-days
    :remote-retention-days
    :minimum-custody
    :restore-verify-every-runs
    :delete-source-after-archive
    :pinned))

(defstruct (archive-policy
            (:constructor %make-policy
                (&key
                 snapshot-every-seconds
                 full-snapshot-every-seconds
                 local-retention-days
                 remote-retention-days
                 minimum-custody
                 restore-verify-every-runs
                 delete-source-after-archive-p
                 pinned-p)))
  snapshot-every-seconds
  full-snapshot-every-seconds
  local-retention-days
  remote-retention-days
  minimum-custody
  restore-verify-every-runs
  delete-source-after-archive-p
  pinned-p)

(defstruct policy-selector
  tenant
  dataset
  dtype
  target-id
  actor-id)

(defstruct archive-context
  tenant
  dataset
  dtype
  target-id
  actor-id)

(defstruct (policy-rule
            (:constructor %make-policy-rule
                (&key id authority selector overrides)))
  id
  authority
  selector
  overrides)

(defstruct resolved-policy
  policy
  digest
  applied-rule-ids
  rejected-rule-ids)

(defun positive-integer-p (value)
  (and (integerp value) (plusp value)))

(defun non-negative-integer-p (value)
  (and (integerp value) (not (minusp value))))

(defun ensure-policy-valid (policy)
  (dolist (pair
           `((:snapshot-every-seconds
              ,(archive-policy-snapshot-every-seconds policy))
             (:full-snapshot-every-seconds
              ,(archive-policy-full-snapshot-every-seconds policy))
             (:local-retention-days
              ,(archive-policy-local-retention-days policy))
             (:remote-retention-days
              ,(archive-policy-remote-retention-days policy))
             (:restore-verify-every-runs
              ,(archive-policy-restore-verify-every-runs policy))))
    (unless (positive-integer-p (second pair))
      (policy-error "~S must be a positive integer, got ~S"
                    (first pair) (second pair))))
  (unless (member (archive-policy-minimum-custody policy)
                  +minimum-custody-values+
                  :test #'eq)
    (policy-error "unsupported minimum custody ~S"
                  (archive-policy-minimum-custody policy)))
  (unless (typep (archive-policy-delete-source-after-archive-p policy)
                 'boolean)
    (policy-error "delete-source-after-archive must be boolean"))
  (unless (typep (archive-policy-pinned-p policy) 'boolean)
    (policy-error "pinned must be boolean"))
  policy)

(defun make-policy (&key
                      (snapshot-every-seconds 3600)
                      (full-snapshot-every-seconds 86400)
                      (local-retention-days 14)
                      (remote-retention-days 365)
                      (minimum-custody :verified-remote)
                      (restore-verify-every-runs 7)
                      (delete-source-after-archive nil)
                      (pinned nil))
  (ensure-policy-valid
   (%make-policy
    :snapshot-every-seconds snapshot-every-seconds
    :full-snapshot-every-seconds full-snapshot-every-seconds
    :local-retention-days local-retention-days
    :remote-retention-days remote-retention-days
    :minimum-custody minimum-custody
    :restore-verify-every-runs restore-verify-every-runs
    :delete-source-after-archive-p delete-source-after-archive
    :pinned-p pinned)))

(defun policy-sexp (policy)
  (list
   :snapshot-every-seconds
   (archive-policy-snapshot-every-seconds policy)
   :full-snapshot-every-seconds
   (archive-policy-full-snapshot-every-seconds policy)
   :local-retention-days
   (archive-policy-local-retention-days policy)
   :remote-retention-days
   (archive-policy-remote-retention-days policy)
   :minimum-custody
   (archive-policy-minimum-custody policy)
   :restore-verify-every-runs
   (archive-policy-restore-verify-every-runs policy)
   :delete-source-after-archive
   (archive-policy-delete-source-after-archive-p policy)
   :pinned
   (archive-policy-pinned-p policy)))

(defun policy-digest (policy)
  (starintel:digest-id
   "starintel.archive.policy/1"
   (with-standard-io-syntax
     (let ((*print-readably* t))
       (prin1-to-string (policy-sexp policy))))))

(defun validate-overrides (overrides)
  (unless (listp overrides)
    (policy-error "policy overrides must be a property list"))
  (unless (evenp (length overrides))
    (policy-error "policy overrides must contain key/value pairs"))
  (loop for (key value) on overrides by #'cddr
        do (declare (ignore value))
           (unless (member key +override-keys+ :test #'eq)
             (policy-error "unknown policy override key ~S" key)))
  overrides)

(defun make-policy-rule (id &key
                              (authority :operator)
                              (selector (make-policy-selector))
                              (overrides nil))
  (check-type id string)
  (unless (member authority '(:operator :actor) :test #'eq)
    (policy-error "unsupported rule authority ~S" authority))
  (unless (policy-selector-p selector)
    (policy-error "selector must be a POLICY-SELECTOR"))
  (%make-policy-rule
   :id id
   :authority authority
   :selector selector
   :overrides (copy-list (validate-overrides overrides))))

(defun selector-value-matches-p (expected actual)
  (or (null expected)
      (and actual (equal expected actual))))

(defun selector-matches-p (selector context)
  (and
   (selector-value-matches-p
    (policy-selector-tenant selector)
    (archive-context-tenant context))
   (selector-value-matches-p
    (policy-selector-dataset selector)
    (archive-context-dataset context))
   (selector-value-matches-p
    (policy-selector-dtype selector)
    (archive-context-dtype context))
   (selector-value-matches-p
    (policy-selector-target-id selector)
    (archive-context-target-id context))
   (selector-value-matches-p
    (policy-selector-actor-id selector)
    (archive-context-actor-id context))))

(defun selector-specificity (selector)
  (+ (if (policy-selector-tenant selector) 10 0)
     (if (policy-selector-dataset selector) 20 0)
     (if (policy-selector-dtype selector) 40 0)
     (if (policy-selector-target-id selector) 80 0)
     (if (policy-selector-actor-id selector) 160 0)))

(defun rule< (left right)
  (let ((left-score (selector-specificity (policy-rule-selector left)))
        (right-score (selector-specificity (policy-rule-selector right))))
    (if (= left-score right-score)
        (string< (policy-rule-id left) (policy-rule-id right))
        (< left-score right-score))))

(defun set-policy-field (policy key value)
  (ecase key
    (:snapshot-every-seconds
     (setf (archive-policy-snapshot-every-seconds policy) value))
    (:full-snapshot-every-seconds
     (setf (archive-policy-full-snapshot-every-seconds policy) value))
    (:local-retention-days
     (setf (archive-policy-local-retention-days policy) value))
    (:remote-retention-days
     (setf (archive-policy-remote-retention-days policy) value))
    (:minimum-custody
     (setf (archive-policy-minimum-custody policy) value))
    (:restore-verify-every-runs
     (setf (archive-policy-restore-verify-every-runs policy) value))
    (:delete-source-after-archive
     (setf (archive-policy-delete-source-after-archive-p policy) value))
    (:pinned
     (setf (archive-policy-pinned-p policy) value)))
  (ensure-policy-valid policy))

(defun apply-operator-rule (policy rule)
  (loop for (key value) on (policy-rule-overrides rule) by #'cddr
        do (set-policy-field policy key value))
  (values policy nil))

(defun actor-custody-safe-p (current proposed)
  (or (eq current proposed)
      (and (eq current :verified-any)
           (eq proposed :verified-remote))))

(defun apply-actor-rule (policy rule)
  (let ((rejected nil))
    (loop for (key value) on (policy-rule-overrides rule) by #'cddr
          do
             (case key
               (:local-retention-days
                (if (and (non-negative-integer-p value)
                         (>= value
                             (archive-policy-local-retention-days policy)))
                    (set-policy-field policy key value)
                    (setf rejected t)))
               (:remote-retention-days
                (if (and (non-negative-integer-p value)
                         (>= value
                             (archive-policy-remote-retention-days policy)))
                    (set-policy-field policy key value)
                    (setf rejected t)))
               (:restore-verify-every-runs
                (if (and (positive-integer-p value)
                         (<= value
                             (archive-policy-restore-verify-every-runs policy)))
                    (set-policy-field policy key value)
                    (setf rejected t)))
               (:minimum-custody
                (if (actor-custody-safe-p
                     (archive-policy-minimum-custody policy)
                     value)
                    (set-policy-field policy key value)
                    (setf rejected t)))
               (:pinned
                (if (eq value t)
                    (set-policy-field policy key t)
                    (setf rejected t)))
               (:delete-source-after-archive
                (if (null value)
                    (set-policy-field policy key nil)
                    (setf rejected t)))
               (otherwise
                (setf rejected t))))
    (values (ensure-policy-valid policy) rejected)))

(defun resolve-policy (base-policy rules context)
  (unless (archive-policy-p base-policy)
    (policy-error "BASE-POLICY must be an ARCHIVE-POLICY"))
  (unless (archive-context-p context)
    (policy-error "CONTEXT must be an ARCHIVE-CONTEXT"))
  (let* ((matching
           (sort
            (remove-if-not
             (lambda (rule)
               (selector-matches-p (policy-rule-selector rule) context))
             (copy-list rules))
            #'rule<))
         (operator-rules
           (remove-if-not
            (lambda (rule) (eq :operator (policy-rule-authority rule)))
            matching))
         (actor-rules
           (remove-if-not
            (lambda (rule) (eq :actor (policy-rule-authority rule)))
            matching))
         (policy (copy-archive-policy base-policy))
         (applied nil)
         (rejected nil))
    (dolist (rule operator-rules)
      (apply-operator-rule policy rule)
      (push (policy-rule-id rule) applied))
    (dolist (rule actor-rules)
      (multiple-value-bind (updated rejected-p)
          (apply-actor-rule policy rule)
        (declare (ignore updated))
        (push (policy-rule-id rule) applied)
        (when rejected-p
          (push (policy-rule-id rule) rejected))))
    (setf applied (nreverse applied)
          rejected (nreverse rejected))
    (make-resolved-policy
     :policy policy
     :digest
     (apply #'starintel:digest-id
            "starintel.archive.resolved-policy/1"
            (policy-digest policy)
            applied)
     :applied-rule-ids applied
     :rejected-rule-ids rejected)))