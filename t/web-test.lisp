(in-package :starintel-test)

(def-suite web-test
  :description "Tests for web-related entities (email, user, breach, email-message)"
  :in starintel-test)

(in-suite web-test)

(test email-creation
  "Test email entity creation"
  (let ((email (make-instance 'email
                              :user "john"
                              :domain "example.com"
                              :password "secret")))
    (is (typep email 'email))
    (is (equal (email-user email) "john"))
    (is (equal (email-domain email) "example.com"))
    (is (equal (email-password email) "secret"))))

(test email-set-id-with-password
  "Test email set-id includes password when present"
  (let ((email1 (make-instance 'email
                               :user "john"
                               :domain "example.com"
                               :password "secret"))
        (email2 (make-instance 'email
                               :user "john"
                               :domain "example.com"
                               :password "")))
    (set-id email1)
    (set-id email2)
    (is (stringp (doc-id email1)))
    (is (stringp (doc-id email2)))
    (is (not (equal (doc-id email1) (doc-id email2))))))

(test email-defaults
  "Test email entity default values"
  (let ((email (make-instance 'email)))
    (is (equal (email-user email) ""))
    (is (equal (email-domain email) ""))
    (is (equal (email-password email) ""))))

(test user-creation
  "Test user entity creation"
  (let ((user (make-instance 'user
                             :name "johndoe"
                             :platform "twitter"
                             :url "https://twitter.com/johndoe")))
    (is (typep user 'user))
    (is (equal (user-name user) "johndoe"))
    (is (equal (user-platform user) "twitter"))
    (is (equal (user-url user) "https://twitter.com/johndoe"))))

(test user-set-id-deterministic
  "Test that user IDs are deterministic based on name, url, platform"
  (let ((user1 (make-instance 'user
                              :name "johndoe"
                              :platform "twitter"
                              :url "https://twitter.com/johndoe"))
        (user2 (make-instance 'user
                              :name "johndoe"
                              :platform "twitter"
                              :url "https://twitter.com/johndoe")))
    (set-id user1)
    (set-id user2)
    (is (equal (doc-id user1) (doc-id user2)))))

(test breach-creation
  "Test breach entity creation"
  (let ((breach (make-instance 'breach
                               :total 1000000
                               :description "Example breach"
                               :url "https://example.com/breach")))
    (is (typep breach 'breach))
    (is (= (breach-total breach) 1000000))
    (is (equal (breach-description breach) "Example breach"))))

(test email-message-creation
  "Test email-message entity creation"
  (let ((msg (make-instance 'email-message
                            :subject "Test Subject"
                            :body "Test Body"
                            :from "sender@example.com"
                            :to "recipient@example.com")))
    (is (typep msg 'email-message))
    (is (equal (email-message-subject msg) "Test Subject"))
    (is (equal (email-message-body msg) "Test Body"))
    (is (equal (email-message-from msg) "sender@example.com"))
    (is (equal (email-message-to msg) "recipient@example.com"))))

(test email-message-set-id-deterministic
  "Test that email-message IDs are deterministic"
  (let ((msg1 (make-instance 'email-message
                             :subject "Test"
                             :body "Body"
                             :from "from@test.com"
                             :to "to@test.com"))
        (msg2 (make-instance 'email-message
                             :subject "Test"
                             :body "Body"
                             :from "from@test.com"
                             :to "to@test.com")))
    (set-id msg1)
    (set-id msg2)
    (is (equal (doc-id msg1) (doc-id msg2)))))

(test new-email-function
  "Test new-email convenience function"
  (let ((email (new-email "test-dataset"
                          :user "john"
                          :domain "example.com")))
    (is (typep email 'email))
    (is (equal (doc-dataset email) "test-dataset"))
    (is (equal (doc-type email) "email"))
    (is (stringp (doc-id email)))))

(test new-email-star-function
  "Test new-email* function that parses email string"
  (let ((email (new-email* "test-dataset" "john@example.com")))
    (is (typep email 'email))
    (is (equal (email-user email) "john"))
    (is (equal (email-domain email) "example.com"))
    (is (equal (doc-dataset email) "test-dataset"))))

(test new-email-star-with-password
  "Test new-email* with password"
  (let ((email (new-email* "test-dataset" "john@example.com"
                           :password "secret")))
    (is (equal (email-password email) "secret"))))

(test new-user-function
  "Test new-user convenience function"
  (let ((user (new-user "test-dataset"
                        :name "johndoe"
                        :platform "twitter")))
    (is (typep user 'user))
    (is (equal (doc-dataset user) "test-dataset"))
    (is (equal (doc-type user) "user"))
    (is (stringp (doc-id user)))))
