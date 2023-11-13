(in-package :starintel)

(defclass booker-message (booker-document)
  ((message :accessor message-message :type string :initarg :message)
   (platform :accessor message-platform :type string :initarg :platform)
   (user :accessor message-user :type string :initarg :user)
   (is-reply :accessor message-is-reply :type bool :initarg :is-reply)
   (media :accessor message-media :type list :initarg :media)
   (message-id :accessor message-message-id :type string :initarg :message-id)
   (reply-to :accessor message-reply-to :type string :initarg :reply-to)
   (group :accessor message-group :type string :initarg :group)
   (channel :accessor message-channel :type string :initarg :channel)
   (mentions :accessor message-mentions :type list :initarg :mentions)))

(defclass booker-social-media-post (booker-document)
  ((content :accessor social-media-post-content :type string :initarg :content)
   (user :accessor social-media-post-user :type string :initarg :user)
   (replies :accessor social-media-post-replies :type list :initarg :replies)
   (media :accessor social-media-post-media :type list :initarg :media)
   (reply-count :accessor social-media-post-reply-count :type int :initarg :reply-count)
   (repost-count :accessor social-media-post-repost-count :type int :initarg :repost-count)
   (url :accessor social-media-post-url :type string :initarg :url)
   (links :accessor social-media-post-links :type list :initarg :links)
   (tags :accessor social-media-post-tags :type list :initarg :tags)
   (title :accessor social-media-post-title :type string :initarg :title)
   (group :accessor social-media-post-group :type string :initarg :group)
   (reply-to :accessor social-media-post-reply-to :type string :initarg :reply-to)))

(defun new-message (message group platform user &optional channel message-id)
  "Create a New Booker Message"
  (let ((message (make-instance 'booker-message :message message :platform platform :user user :is-reply nil :media '() :message-id (or message-id "") :reply-to "" :group group :channel (or channel "") :mentions '())))
    (hash-id message (format nil "~a~a~a~a" message group platform user))
    message))

(defun new-social-media-post (user content title group &optional url date)
  "Create a New Booker Social Media Post"
  (let ((post (make-instance 'booker-social-media-post :content content :user user :replies '() :media '() :reply-count 0 :repost-count 0 :url (or url "") :links '() :tags '() :title title :group group :reply-to "")))
    (hash-id post (format nil "~a~a~a~a" user content title group))
    post))
