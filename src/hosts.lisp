(in-package :starintel)

(defclass booker-web (booker-document)
  ((source :accessor web-source :type string :initarg :source)))

(defclass booker-domain (booker-web)
  ((record-type :accessor domain-record-type :type string :initarg :record-type)
   (record :accessor domain-record :type string :initarg :record)
   (ip :accessor domain-ip :type string :initarg :ip)))

(defclass booker-port ()
  ((number :accessor port-number :type int16 :initarg :number)
   (services :accessor port-services :type list :initarg :services)))

(defclass booker-asn ()
  ((number :accessor asn-number :type int32 :initarg :number)
   (subnet :accessor asn-subnet :type string :initarg :subnet)))

(defclass booker-network (booker-web)
  ((org :accessor network-org :type string :initarg :org)
   (asn :accessor network-asn :type booker-asn :initarg :asn)))

(defclass booker-host (booker-web)
  ((hostname :accessor host-hostname :type string :initarg :hostname)
   (ip :accessor host-ip :type string :initarg :ip)
   (ports :accessor host-ports :type list :initarg :ports)
   (os :accessor host-os :type string :initarg :os)))

(defclass booker-url (booker-web)
  ((url :accessor url-url :type string :initarg :url)
   (content :accessor url-content :type string :initarg :content)))

(defun new-domain (domain record-type &optional ip)
  "Create a New Booker Domain"
  (let ((domain (make-instance 'booker-domain :record-type record-type :record domain :ip ip)))
    (hash-id domain (format nil "~a~a~a" domain record-type (or ip "")))
    domain))

(defun new-port (number &optional services)
  "Create a New Booker Port"
  (make-instance 'booker-port :number number :services (or services '())))

(defun new-asn (number subnet)
  "Create a New Booker ASN"
  (make-instance 'booker-asn :number number :subnet subnet))

(defun new-network (asn org)
  "Create a New Booker Network"
  (make-instance 'booker-network :asn asn :org org))

(defun new-host (ip &optional hostname)
  "Create a New Booker Host"
  (let ((host (make-instance 'booker-host :ip ip :hostname hostname)))
    (hash-id host (format nil "~a~a" (or hostname "") ip))
    host))

(defun new-url (url content)
  "Create a New Booker URL"
  (make-instance 'booker-url :url url :content content))
