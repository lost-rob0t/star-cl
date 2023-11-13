(in-package :starintel)

;; TODO Define some common errors for missing fields

(defclass booker-entity (booker-document)
  ((etype :accessor doc-etype :type string :initarg :etype)
   (eid :accessor doc-eid :type string :initarg :eid)))

(defclass booker-person (booker-entity)
  ((fname :accessor person-fname :type string :initarg :fname :initform (error "Person requires fname"))
   (mname :accessor person-mname :type string :initarg :mname :initform "")
   (lname :accessor person-lname :type string :initarg :lname :initform (error "Person requires lname"))
   (bio :initarg person-bio :type string :initarg :bio)
   (dob :initarg person-dob :type string :initarg :dob)
   (race :initarg person-race :type string :initarg :race)
   (region :initarg person-region :type string :initarg :region)
   (misc :initarg person-misc :type list :initarg :misc)))

(defclass booker-org (booker-entity)
  ((reg :accessor org-reg :type string :initarg :reg)
   (name :accessor org-name :type string :initarg :name)
   (bio :accessor org-bio :type string :initarg :bio)
   (country :accessor org-country :type string :initarg :country)
   (website :accessor org-website :type string :initarg :website)))



(defun new-org (name etype &rest args)
  "Create a New Booker Organization"
  (let ((org (apply #'make-instance 'booker-org :name name :etype etype args)))
    (hash-id org (format nil "~a~a" name etype))
    org))
