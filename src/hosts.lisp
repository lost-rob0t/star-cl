(in-package :starintel)

(defclass domain (document)
  ((record-type :accessor domain-record-type :type string :initarg :record-type :initform (error "Domain record type is required"))
   (record :accessor domain-record :type string :initarg :record :initform (error "Domain record is required"))
   (ip :accessor domain-ip :type string :initarg :ip)))

(defclass port ()
  ((number :accessor port-number :type int16 :initarg :number)
   (services :accessor port-services :type list :initarg :services)))

(defclass asn ()
  ((number :accessor asn-number :type int32 :initarg :number)
   (subnet :accessor asn-subnet :type string :initarg :subnet)))

(defclass network (document)
  ((org :accessor network-org :type string :initarg :org :initform (error "Network org name is required"))
   (asn :accessor network-asn :type asn :initarg :asn :initform (error "Network ASN is required"))))

(defclass host (document)
  ((hostname :accessor host-hostname :type string :initarg :hostname :initform (error "Hostname is required"))
   (ip :accessor host-ip :type string :initarg :ip :initform (error "IP address is required"))
   (ports :accessor host-ports :type list :initarg :ports)
   (os :accessor host-os :type string :initarg :os)))

(defclass url (document)
  ((url :accessor url-url :type string :initarg :url :initform (error "URL is required"))
   (content :accessor url-content :type string :initarg :content)))

(defmethod set-id ((doc domain))
  "Set the ID for a domain document"
  (hash-id doc (domain-record doc) (domain-record-type doc) (domain-ip doc)))

(defmethod set-id ((doc port))
  "Set the ID for a port document"
  (hash-id doc (port-number doc) (port-services doc)))

(defmethod set-id ((doc asn))
  "Set the ID for an ASN document"
  (hash-id doc (asn-number doc) (asn-subnet doc)))

(defmethod set-id ((doc network))
  "Set the ID for a network document"
  (hash-id doc (network-org doc) (network-asn doc)))

(defmethod set-id ((doc host))
  "Set the ID for a host document"
  (hash-id doc (host-hostname doc) (host-ip doc)))

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
