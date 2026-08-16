(in-package :starintel)

(defclass domain (document)
  ((record-type :accessor domain-record-type :type string :initarg :record-type :initform "")
   (record :accessor domain-record :type string :initarg :record :initform "")
   (resolved-addresses :accessor domain-resolved :type list :initarg :resolved :initform '())))

(defclass service ()
  ((port :accessor service-port :type integer :initarg :number)
   (name :accessor name :type string :initarg :services)
   (ver :accessor version :type string :initarg :version)))

(defclass network (document)
  ((org :accessor network-org :type string :initarg :org :initform "")
   (subnet :accessor network-subnet :type string :initarg :subnet :initform "")
   (asn :accessor network-asn :type integer :initarg :asn :initform 0)))

(defclass host (document)
  ((hostname :accessor host-hostname :type string :initarg :hostname :initform "")
   (ip :accessor host-ip :type string :initarg :ip :initform "")
   (os :accessor host-os :type string :initarg :os :initform "")
   (ports :accessor host-ports :type list :initarg :ports :initform nil)))

(defclass url (document)
  ((url :accessor url-uri :type string :initarg :url :initform "")
   (path :accessor url-path :type string :initarg :path :initform "")
   (query :accessor url-query :type string :initarg :query :initform "")
   (content :accessor url-content :type string :initarg :content :initform "")))

(defun url-url (document)
  "Compatibility accessor for older callers."
  (url-uri document))

(defun (setf url-url) (value document)
  (setf (url-uri document) value))

(defun canonical-hostname-for-id (hostname)
  "Return HOSTNAME normalized for unresolved-host identity.

Host identity is case-insensitive and ignores a DNS trailing dot. The stored
hostname itself is not rewritten by this function."
  (string-right-trim
   '(#\.)
   (string-downcase
    (string-trim '(#\Space #\Tab #\Newline #\Return) (or hostname "")))))

(defmethod set-id ((doc domain))
  "Set the deterministic domain ID when no ID exists."
  (when (document-id-missing-p doc)
    (hash-id doc (domain-record doc) (domain-record-type doc)))
  (doc-id doc))

(defmethod set-id ((doc network))
  "Set the deterministic network ID when no ID exists."
  (when (document-id-missing-p doc)
    (hash-id doc (network-asn doc) (network-org doc)))
  (doc-id doc))

(defmethod set-id ((doc host))
  "Set deterministic host ID while preserving resolved-host compatibility.

A non-empty IP keeps the historical IP-only identity exactly. If IP is empty,
fall back to a tagged, normalized hostname identity. A host with neither value
is not a valid canonical document and is rejected instead of collapsing onto
the digest of an empty string."
  (when (document-id-missing-p doc)
    (let* ((ip (host-ip doc))
           (hostname (canonical-hostname-for-id (host-hostname doc))))
      (cond
        ((plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) ip)))
         ;; Deliberately hash the original IP string to preserve existing IDs.
         (hash-id doc ip))
        ((plusp (length hostname))
         (hash-id doc "hostname" hostname))
        (t
         (error "Host requires a non-empty IP address or hostname for identity.")))))
  (doc-id doc))

(defmethod set-id ((doc url))
  "Set the deterministic URL ID when no ID exists."
  (when (document-id-missing-p doc)
    (hash-id doc (url-uri doc) (url-content doc)))
  (doc-id doc))

(defun new-domain (dataset &rest args)
  "Create a New Domain"
  (let ((domain (apply #'make-instance 'domain args)))
    (set-meta domain dataset)
    domain))

(defun new-port (dataset &rest args)
  "Create a New Port"
  (let ((port (apply #'make-instance 'port args)))
    (set-meta port dataset)
    port))

(defun new-asn (dataset &rest args)
  "Create a New ASN"
  (let ((asn (apply #'make-instance 'asn args)))
    (set-meta asn dataset)
    asn))

(defun new-network (dataset &rest args)
  "Create a New Network"
  (let ((network (apply #'make-instance 'network args)))
    (set-meta network dataset)
    network))

(defun new-host (dataset &rest args)
  "Create a New Host"
  (let ((host (apply #'make-instance 'host args)))
    (set-meta host dataset)
    host))

(defun new-url (dataset &rest args)
  "Create a New URL"
  (let ((url (apply #'make-instance 'url args)))
    (set-meta url dataset)
    url))
