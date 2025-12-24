(in-package :starintel-test)

(def-suite define-test
  :description "Tests for high-level document definition functions"
  :in starintel-test)

(in-suite define-test)

(test create-function
  "Test the create function"
  (let ((doc (create 'document :dataset "test")))
    (is (typep doc 'document))
    (is (equal (doc-dataset doc) "test"))))

(test arrow-creates-relation
  "Test that -> function creates a relation between two documents"
  (let* ((result (-> (list #'new-person "ds1" "John" "Doe" "person")
                     (list #'new-org "ds2" "ACME Corp" "org")
                     "works at"))
         (source-doc (first result))
         (target-doc (second result))
         (relation (third result)))
    (is (= (length result) 3))
    (is (typep source-doc 'person))
    (is (typep target-doc 'org))
    (is (typep relation 'relation))
    (is (equal (relation-source relation) (doc-id source-doc)))
    (is (equal (relation-target relation) (doc-id target-doc)))
    (is (equal (relation-note relation) "works at"))))

(test arrow-without-note
  "Test -> function without a note"
  (let* ((result (-> (list #'new-person "ds1" "John" "Doe" "person")
                     (list #'new-org "ds2" "ACME Corp" "org")))
         (relation (third result)))
    (is (typep relation 'relation))
    (is (null (relation-note relation)))))

(test define-docs-creates-documents
  "Test define-docs creates multiple documents"
  (let ((docs (define-docs
                (list #'new-person "ds1" "John" "Doe" "person")
                (list #'new-org "ds2" "ACME Corp" "org"))))
    (is (= (length docs) 2))
    (is (typep (first docs) 'person))
    (is (typep (second docs) 'org))))

(test define-docs-with-arrow
  "Test define-docs with arrow relationships"
  (let ((docs (define-docs
                (list #'new-person "ds1" "John" "Doe" "person")
                (list #'-> (list #'new-person "ds1" "Jane" "Smith" "person")
                      (list #'new-org "ds2" "ACME Corp" "org")
                      "works at"))))
    (is (>= (length docs) 2))
    ;; First should be a person
    (is (typep (first docs) 'person))
    ;; Arrow should produce multiple documents (source, target, relation)
    (is (some (lambda (doc) (typep doc 'relation)) docs))))

(test define-docs-flattens-results
  "Test that define-docs flattens nested lists from arrow function"
  (let ((docs (define-docs
                (list #'-> (list #'new-person "ds1" "John" "Doe" "person")
                      (list #'new-org "ds2" "ACME" "org")
                      "works at"))))
    ;; Arrow returns a list of 3 items, should be flattened
    (is (= (length docs) 3))
    (is (typep (first docs) 'person))
    (is (typep (second docs) 'org))
    (is (typep (third docs) 'relation))))
