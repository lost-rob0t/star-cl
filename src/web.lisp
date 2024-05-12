(in-package :starintel)

(defclass breach (document)
  ((total :accessor breach-total :type int :initarg :total :initform 0)
   (description :accessor breach-description :type string :initarg :description :initform "")
   (url :accessor breach-url :type string :initarg :url :initform "")))

(defclass email (document)
  ((user :accessor email-user :type string :initarg :user :initform "")
   (domain :accessor email-domain :type string :initarg :domain :initform "")
   (password :accessor email-password :type string :initarg :password :initform "")))



(defclass email-message (document)
  ((body :accessor email-message-body :type string :initarg :body :initform "")
   (subject :accessor email-message-subject :type string :initarg :subject :initform "")
   (to :accessor email-message-to :type string :initarg :to :initform "")
   (from :accessor email-message-from :type string :initarg :from :initform "")
   (headers :accessor email-message-headers :type string :initarg :headers :initform "")
   (cc :accessor email-message-cc :type list :initarg :cc :initform '())
   (bcc :accessor email-message-bcc :type list :initarg :bcc :initform '())))

(defclass user (document)
  ((url :accessor user-url :type string :initarg :url :initform (error "user requires a url"))
   (name :accessor user-name :type string :initarg :name :initform (error "user requires a name."))
   (platform :accessor user-platform :type string :initarg :platform :initform (error "user requires a platform."))
   (misc :accessor user-misc :type list :initarg :misc :initform '())
   (bio :accessor user-bio :type string :initarg :bio :initform "")))


(defmethod set-id ((doc email))
  (hash-id doc
           (email-user doc)
           (email-domain doc)
           (when (> (length (email-password doc)) 0)
             (email-password doc))))


(defmethod set-id ((doc user))
  (hash-id doc
           (user-name doc)
           (user-url doc)
           (user-platform doc)))



(defmethod set-id ((doc email-message))
  (hash-id doc (email-message-body doc)
           (email-message-to doc)
           (email-message-from doc)
           (email-message-subject doc)))

;; TODO make a function that takes user and domain and use that to make new-email*
(defun new-email (dataset &rest args)
  "Create a New Booker Email"
  (let ((email (apply #'make-instance 'email args)))
    (set-meta email dataset)
    email))

(defun new-email* (dataset email &rest args)
  "Create a New Booker Email"
  (let* ((email-data (uiop:split-string email :max 2 :separator "@"))
         (user (nth 0 email-data))
         (domain (nth 1 email-data))
         (email (apply #'make-instance 'email :user user :domain domain args)))
    (set-meta email dataset)
    email))


(defun new-user (dataset &rest args)
  "Create a New Booker User"
  (let ((user (apply #'make-instance 'user args)))
    (set-id user)
    (set-meta user dataset)
    user))
