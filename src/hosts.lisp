(in-package :starintel)

(defclass domain (document)
  ((record-type :accessor domain-record-type :type string :initarg :record-type :initform "")
   (record :accessor domain-record :type string :initarg :record :initform "")
   (resolved-addresses :accessor domain-resolved :type list :initarg :resolved)))

(defclass service ()
  ((port :accessor service-port :type integer :initarg :number)
   (name :accessor name :type string :initarg :services)
   (ver :accessor version :type string :initarg :version)))

(defclass network (document)
  ((org :accessor network-org :type string :initarg :org :initform "")
   (subnet :accessor network-asn :type string :initarg :subnet :initform "")
   (asn :accessor network-asn :type integer :initarg :asn :initform "")))

;; TODO set empty strins
(defclass host (document)
  ((hostname :accessor host-hostname :type string :initarg :hostname :initform "")
   (ip :accessor host-ip :type string :initarg :ip :initform "")
   (os :accessor host-os :type string :initarg :os)
   ;;; Should be list of <service>
   (ports :accessor host-ports :type list :initarg :ports)))

(defclass url (document)
  ((url :accessor url-uri :type string :initarg :url :initform "")
   (path :accessor url-path :type string :initarg :path :initform "")
   (content :accessor url-content :type string :initarg :content)))

(defmethod set-id ((doc domain))
  "Set the ID for a domain document"
  (hash-id doc (domain-record doc) (domain-record-type doc)))

(defmethod set-id ((doc network))
  "Set the ID for a network document"
  (hash-id doc (network-asn doc) (network-org doc)))

(defmethod set-id ((doc host))
  "Set the ID for a host document"
  (hash-id doc (host-ip doc)))

(defmethod set-id ((doc url))
  "Set the ID for a URL document"
  (hash-id doc (url-url doc) (url-content doc)))

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
