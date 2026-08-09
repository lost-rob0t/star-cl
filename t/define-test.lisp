(in-package :starintel-test)

(def-suite define-test
  :description "Tests for high-level document definition functions"
  :in starintel-test)

(in-suite define-test)

(test create-function
  "Test the create function"
  (let ((document (create 'document :dataset "test")))
    (is (typep document 'document))
    (is (equal (doc-dataset document) "test"))))

(test arrow-creates-relation
  "Test that -> function creates a relation between two documents"
  (let* ((result (-> (list #'new-person "ds1" "John" "Doe" "person")
                     (list #'new-org "ds2" "ACME Corp" "org")
                     "works at"))
         (source-document (first result))
         (target-document (second result))
         (relation (third result)))
    (is (= (length result) 3))
    (is (typep source-document 'person))
    (is (typep target-document 'org))
    (is (typep relation 'relation))
    (is (equal (relation-source relation) (doc-id source-document)))
    (is (equal (relation-target relation) (doc-id target-document)))
    (is (equal (relation-note relation) "works at"))))

(test arrow-without-note
  "Test -> function without a note"
  (let* ((result (-> (list #'new-person "ds1" "John" "Doe" "person")
                     (list #'new-org "ds2" "ACME Corp" "org")))
         (relation (third result)))
    (is (typep relation 'relation))
    (is (string= "" (relation-note relation)))))

(test define-docs-creates-documents
  "Test define-docs creates multiple documents"
  (let ((documents
          (define-docs
           (list #'new-person "ds1" "John" "Doe" "person")
           (list #'new-org "ds2" "ACME Corp" "org"))))
    (is (= (length documents) 2))
    (is (typep (first documents) 'person))
    (is (typep (second documents) 'org))))

(test define-docs-with-arrow
  "Test define-docs with arrow relationships"
  (let ((documents
          (define-docs
           (list #'new-person "ds1" "John" "Doe" "person")
           (list #'->
                 (list #'new-person "ds1" "Jane" "Smith" "person")
                 (list #'new-org "ds2" "ACME Corp" "org")
                 "works at"))))
    (is (>= (length documents) 2))
    (is (typep (first documents) 'person))
    (is (some (lambda (document) (typep document 'relation)) documents))))

(test define-docs-flattens-results
  "Test that define-docs flattens nested lists from arrow function"
  (let ((documents
          (define-docs
           (list #'->
                 (list #'new-person "ds1" "John" "Doe" "person")
                 (list #'new-org "ds2" "ACME" "org")
                 "works at"))))
    (is (= (length documents) 3))
    (is (typep (first documents) 'person))
    (is (typep (second documents) 'org))
    (is (typep (third documents) 'relation))))
