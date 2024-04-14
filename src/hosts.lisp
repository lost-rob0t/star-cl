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

(defun new-domain (domain record-type &optional ip)
  "Create a New Booker Domain"
  (let ((domain (make-instance 'domain :record-type record-type :record domain :ip ip)))
    (hash-id domain (format nil "~a~a~a" domain record-type (or ip "")))
    domain))

(defun new-port (number &optional services)
  "Create a New Booker Port"
  (make-instance 'port :number number :services (or services '())))

(defun new-asn (number subnet)
  "Create a New Booker ASN"
  (make-instance 'asn :number number :subnet subnet))

(defun new-network (asn org)
  "Create a New Booker Network"
  (make-instance 'network :asn asn :org org))

(defun new-host (ip &optional hostname)
  "Create a New Booker Host"
  (let ((host (make-instance 'host :ip ip :hostname hostname)))
    (hash-id host (format nil "~a~a" (or hostname "") ip))
    host))

(defun new-url (url content)
  "Create a New Booker URL"
  (make-instance 'url :url url :content content))
