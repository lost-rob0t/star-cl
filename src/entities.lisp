(in-package :starintel)

;; TODO Define some common errors for missing fields

;; WARNING DEVIATION FROM SPEC!!!!!!!

;; (defclass entity (document)
;;   ())

(defclass person (document)
  ((fname :accessor person-fname :type string :initarg :fname :initform (error "Person requires fname"))
   (mname :accessor person-mname :type string :initarg :mname :initform "")
   (lname :accessor person-lname :type string :initarg :lname :initform (error "Person requires lname"))
   (bio :initarg person-bio :type string :initarg :bio :initform "")
   (dob :initarg person-dob :type string :initarg :dob :initform "")
   (race :initarg person-race :type string :initarg :race :initform "")
   (region :initarg person-region :type string :initarg :region :initform "")
   (misc :initarg person-misc :type list :initarg :misc :initform '())
   ;; HACK Fix this, infact i should move the main doc to a metadata field
   (etype :accessor doc-etype :type string :initarg :etype :initform "")

   (eid :accessor doc-eid :type string :initarg :eid :initform "")))


(defclass org (document)
  ((reg :accessor org-reg :type string :initarg :reg :initform "")
   (name :accessor org-name :type string :initarg :name :initform (error "Org requires name"))
   (bio :accessor org-bio :type string :initarg :bio :initform "")
   (country :accessor org-country :type string :initarg :country :initform "")
   (website :accessor org-website :type string :initarg :website :initform "")
   (etype :accessor doc-etype :type string :initarg :etype :initform "")
   (eid :accessor doc-eid :type string :initarg :eid :initform "")))




(defun new-org (name etype &rest args)
  "Create a New Booker Organization"
  (let ((org (apply #'make-instance 'org :name name :etype etype args)))
    (hash-id org (format nil "~a~a" name etype))
    org))
