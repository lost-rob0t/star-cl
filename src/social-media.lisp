(in-package :starintel)

(defclass message (document)
  ((message :accessor message-content :type string :initarg :content :initform "")
   (platform :accessor message-platform :type string :initarg :platform :initform "")
   (user :accessor message-user :type string :initarg :user :initform "")
   (is-reply :accessor message-is-reply :type bool :initarg :is-reply :initform nil)
   (media :accessor message-media :type list :initarg :media :initform '())
   (message-id :accessor message-id :type string :initarg :message-id :initform "")
   (reply-to :accessor message-reply-to :type string :initarg :reply-to :initform "")
   (group :accessor message-group :type string :initarg :group :initform "")
   (channel :accessor message-channel :type string :initarg :channel :initform "")
   (mentions :accessor message-mentions :type list :initarg :mentions :initform '())))

(defclass socialmpost (document)
  ((content :accessor social-media-post-content :type string :initarg :content :initform "")
   (user :accessor social-media-post-user :type string :initarg :user :initform "")
   (replies :accessor social-media-post-replies :type list :initarg :replies :initform '())
   (media :accessor social-media-post-media :type list :initarg :media :initform '())
   (reply-count :accessor social-media-post-reply-count :type int :initarg :reply-count :initform 0)
   (repost-count :accessor social-media-post-repost-count :type int :initarg :repost-count :initform 0)
   (url :accessor social-media-post-url :type string :initarg :url :initform "")
   (links :accessor social-media-post-links :type list :initarg :links :initform '())
   (tags :accessor social-media-post-tags :type list :initarg :tags :initform '())
   (title :accessor social-media-post-title :type string :initarg :title :initform "")
   (group :accessor social-media-post-group :type string :initarg :group :initform "")
   (reply-to :accessor social-media-post-reply-to :type string :initarg :reply-to :initform "")))


(defmethod set-id ((doc message))
  (hash-id doc
           (message-content doc)
           (message-user doc)
           (message-channel doc)
           (message-group doc)
           (message-id doc)
           (message-platform doc)))

(defmethod set-id ((doc socialmpost))
  (hash-id doc
           (social-media-post-content doc)
           (social-media-post-user doc)
           (social-media-post-url doc)
           (social-media-post-group doc)))




(defun new-message (dataset &rest args)
  "Create a New Booker Message"
  (let ((message (apply #'make-instance 'message args)))
    (set-meta message dataset)
    message))

(defun new-social-media-post (dataset &rest args)
  "Create a New Booker Social Media Post"
  (let ((post (apply #'make-instance 'socialmpost args)))
    (set-meta post dataset)
    post))
