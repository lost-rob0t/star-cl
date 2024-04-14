(in-package :starintel)

(defclass breach (document)
  ((total :accessor breach-total :type int :initarg :total)
   (description :accessor breach-description :type string :initarg :description)
   (url :accessor breach-url :type string :initarg :url)))

(defclass email (document)
  ((user :accessor email-user :type string :initarg :user)
   (domain :accessor email-domain :type string :initarg :domain)
   (password :accessor email-password :type string :initarg :password)))

(defclass email-message (document)
  ((body :accessor email-message-body :type string :initarg :body)
   (subject :accessor email-message-subject :type string :initarg :subject)
   (to :accessor email-message-to :type string :initarg :to)
   (from :accessor email-message-from :type string :initarg :from)
   (headers :accessor email-message-headers :type string :initarg :headers)
   (cc :accessor email-message-cc :type list :initarg :cc)
   (bcc :accessor email-message-bcc :type list :initarg :bcc)))

(defclass user (document)
  ((url :accessor user-url :type string :initarg :url)
   (name :accessor user-name :type string :initarg :name)
   (platform :accessor user-platform :type string :initarg :platform)
   (misc :accessor user-misc :type list :initarg :misc)
   (bio :accessor user-bio :type string :initarg :bio)))

(defun new-email (email)
  "Create a New Booker Email"
  (let* ((email-data (split-string email "@"))
         (user (nth 0 email-data))
         (domain (nth 1 email-data))
         (email (make-instance 'email :user user :domain domain)))
    (hash-id email (format nil "~a~a" user domain))
    email))

(defun new-email-with-password (user domain password)
  "Create a New Booker Email with Password"
  (let ((email (make-instance 'email :user user :domain domain :password password)))
    (hash-id email (format nil "~a~a~a" user domain password))
    email))

(defun new-user (username platform &optional url)
  "Create a New Booker User"
  (let ((user (make-instance 'user :name username :platform platform :url (or url "") :dtype "user")))
    (hash-id user (format nil "~a~a" username (or url "")))
    user))

(defun new-username (username platform &optional url)
  "Create a New Booker User with Username"
  (new-user username platform url))
