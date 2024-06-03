(in-package :starintel)


;; WARNING DEVIATION FROM SPEC!!!!!!!

;; (defclass entity (document)
;;   ())

(defclass person (document)
  ((fname :accessor person-fname :type string :initarg :fname :initform "")
   (mname :accessor person-mname :type string :initarg :mname :initform "")
   (lname :accessor person-lname :type string :initarg :lname :initform "")
   (bio :initarg person-bio :type string :initarg :bio :initform "")
   (dob :initarg person-dob :type string :initarg :dob :initform "")
   (race :initarg person-race :type string :initarg :race :initform "")
   (region :initarg person-region :type string :initarg :region :initform "")
   (misc :initarg person-misc :type list :initarg :misc :initform '())
   ;; HACK Fix this, infact i should move the main doc to a metadata field
   (etype :accessor doc-etype :type string :initarg :etype :initform "person")
   (eid :accessor doc-eid :type string :initarg :eid :initform "")))


(defclass org (document)
  ((reg :accessor org-reg :type string :initarg :reg :initform "")
   (name :accessor org-name :type string :initarg :name :initform "")
   (bio :accessor org-bio :type string :initarg :bio :initform "")
   (country :accessor org-country :type string :initarg :country :initform "")
   (website :accessor org-website :type string :initarg :website :initform "")
   (etype :accessor doc-etype :type string :initarg :etype :initform "org")
   (eid :accessor doc-eid :type string :initarg :eid :initform "")))


(defmethod set-id ((doc person))
  "Set the ID for a person document"
  (ulid-id doc))

(defmethod set-id ((doc org))
  "Set the ID for an organization document"
  (hash-id doc (org-name doc) (org-reg doc) (org-country doc)))

(defun new-org (dataset name etype &rest args)
  "Create a New Booker Organization"
  (let ((org (apply #'make-instance 'org :name name :etype etype args)))
    (set-meta org dataset)
    org))

(defun new-person (dataset fname lname etype &rest args)
  "Create a New Booker Person"
  (let ((person (apply #'make-instance 'person :fname fname :lname lname :etype etype args)))
    (set-meta person dataset)
    person))
