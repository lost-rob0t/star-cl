(in-package :starintel-test)

(def-suite relations-test
  :description "Tests for relation entities"
  :in starintel-test)

(in-suite relations-test)

(test relation-creation
  "Test relation entity creation"
  (let ((relation (make-instance 'relation
                                 :source "source-id"
                                 :target "target-id"
                                 :note "test relation")))
    (is (typep relation 'relation))
    (is (equal (relation-source relation) "source-id"))
    (is (equal (relation-target relation) "target-id"))
    (is (equal (relation-note relation) "test relation"))))

(test relation-defaults
  "Test relation entity default values"
  (let ((relation (make-instance 'relation)))
    (is (equal (relation-source relation) ""))
    (is (equal (relation-target relation) ""))
    (is (equal (relation-note relation) ""))))

(test relation-set-id-uses-ulid
  "Test that relation set-id uses ULID"
  (let ((relation (make-instance 'relation)))
    (set-id relation)
    (is (stringp (doc-id relation)))
    (is (> (length (doc-id relation)) 0))))

(test relation-ids-are-unique
  "Test that relations get unique IDs (ULIDs)"
  (let ((rel1 (make-instance 'relation
                             :source "source-id"
                             :target "target-id"))
        (rel2 (make-instance 'relation
                             :source "source-id"
                             :target "target-id")))
    (set-id rel1)
    (set-id rel2)
    (is (not (equal (doc-id rel1) (doc-id rel2))))))

(test new-relation-function
  "Test new-relation convenience function"
  (let ((relation (new-relation "test-dataset"
                                "source-id"
                                "target-id"
                                "test note")))
    (is (typep relation 'relation))
    (is (equal (relation-source relation) "source-id"))
    (is (equal (relation-target relation) "target-id"))
    (is (equal (relation-note relation) "test note"))
    (is (equal (doc-dataset relation) "test-dataset"))
    (is (equal (doc-type relation) "relation"))
    (is (stringp (doc-id relation)))))

(test relation-is-document
  "Test that relation is a subclass of document"
  (let ((relation (make-instance 'relation)))
    (is (typep relation 'document))))
