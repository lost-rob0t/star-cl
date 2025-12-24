(in-package :starintel-test)

(def-suite entities-test
  :description "Tests for person and organization entities"
  :in starintel-test)

(in-suite entities-test)

(test person-creation
  "Test person entity creation"
  (let ((person (make-instance 'person
                               :fname "John"
                               :lname "Doe"
                               :etype "person")))
    (is (typep person 'person))
    (is (equal (person-fname person) "John"))
    (is (equal (person-lname person) "Doe"))
    (is (equal (doc-etype person) "person"))))

(test person-defaults
  "Test person entity default values"
  (let ((person (make-instance 'person)))
    (is (equal (person-fname person) ""))
    (is (equal (person-mname person) ""))
    (is (equal (person-lname person) ""))))

(test org-creation
  "Test organization entity creation"
  (let ((org (make-instance 'org
                            :name "ACME Corp"
                            :country "US"
                            :etype "org")))
    (is (typep org 'org))
    (is (equal (org-name org) "ACME Corp"))
    (is (equal (org-country org) "US"))
    (is (equal (doc-etype org) "org"))))

(test org-defaults
  "Test organization entity default values"
  (let ((org (make-instance 'org)))
    (is (equal (org-reg org) ""))
    (is (equal (org-name org) ""))
    (is (equal (org-bio org) ""))
    (is (equal (org-country org) ""))
    (is (equal (org-website org) ""))))

(test person-set-id-uses-ulid
  "Test that person set-id uses ULID"
  (let ((person (make-instance 'person)))
    (set-id person)
    (is (stringp (doc-id person)))
    (is (> (length (doc-id person)) 0))))

(test org-set-id-uses-hash
  "Test that org set-id uses hash-based ID"
  (let ((org (make-instance 'org
                            :name "ACME Corp"
                            :reg "12345"
                            :country "US")))
    (set-id org)
    (is (stringp (doc-id org)))
    (is (= (length (doc-id org)) 32)))) ; MD5 hash

(test org-hash-id-deterministic
  "Test that org IDs are deterministic based on name, reg, country"
  (let ((org1 (make-instance 'org
                             :name "ACME Corp"
                             :reg "12345"
                             :country "US"))
        (org2 (make-instance 'org
                             :name "ACME Corp"
                             :reg "12345"
                             :country "US")))
    (set-id org1)
    (set-id org2)
    (is (equal (doc-id org1) (doc-id org2)))))

(test new-org-function
  "Test new-org convenience function"
  (let ((org (new-org "test-dataset" "ACME Corp" "org"
                      :country "US" :reg "12345")))
    (is (typep org 'org))
    (is (equal (org-name org) "ACME Corp"))
    (is (equal (doc-dataset org) "test-dataset"))
    (is (equal (doc-type org) "org"))
    (is (stringp (doc-id org)))))

(test new-person-function
  "Test new-person convenience function"
  (let ((person (new-person "test-dataset" "John" "Doe" "person"
                            :mname "Q")))
    (is (typep person 'person))
    (is (equal (person-fname person) "John"))
    (is (equal (person-lname person) "Doe"))
    (is (equal (person-mname person) "Q"))
    (is (equal (doc-dataset person) "test-dataset"))
    (is (equal (doc-type person) "person"))
    (is (stringp (doc-id person)))))

(test person-is-document
  "Test that person is a subclass of document"
  (let ((person (make-instance 'person)))
    (is (typep person 'document))))

(test org-is-document
  "Test that org is a subclass of document"
  (let ((org (make-instance 'org)))
    (is (typep org 'document))))
