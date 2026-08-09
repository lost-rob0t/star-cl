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
  (let ((relation-1 (make-instance 'relation
                                   :source "source-id"
                                   :target "target-id"))
        (relation-2 (make-instance 'relation
                                   :source "source-id"
                                   :target "target-id")))
    (set-id relation-1)
    (set-id relation-2)
    (is (not (equal (doc-id relation-1) (doc-id relation-2))))))

(test new-relation-function
  "Test new-relation convenience function"
  (let ((relation (new-relation "test-dataset"
                                "source-id"
                                "target-id"
                                :note "test note")))
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

(test relation-subject-object-from-source-target
  "Legacy source/target populate canonical subject/object"
  (let ((relation (make-instance 'relation
                                 :source "src-id"
                                 :target "tgt-id")))
    (is (equal (relation-subject relation) "src-id"))
    (is (equal (relation-object relation) "tgt-id"))
    (is (equal (relation-source relation) "src-id"))
    (is (equal (relation-target relation) "tgt-id"))))

(test relation-subject-object-explicit
  "Explicit subject/object populate legacy source/target"
  (let ((relation (make-instance 'relation
                                 :subject "subj-id"
                                 :object "obj-id")))
    (is (equal (relation-subject relation) "subj-id"))
    (is (equal (relation-object relation) "obj-id"))
    (is (equal (relation-source relation) "subj-id"))
    (is (equal (relation-target relation) "obj-id"))))

(test relation-subject-object-precedence
  "Explicit subject/object take precedence over source/target"
  (let ((relation (make-instance 'relation
                                 :source "legacy-src"
                                 :target "legacy-tgt"
                                 :subject "canon-subj"
                                 :object "canon-obj")))
    (is (equal (relation-subject relation) "canon-subj"))
    (is (equal (relation-object relation) "canon-obj"))
    (is (equal (relation-source relation) "legacy-src"))
    (is (equal (relation-target relation) "legacy-tgt"))))

(test new-relation-with-subject-object
  "new-relation accepts :subject/:object keywords"
  (let ((relation (new-relation "ds" "legacy-src" "legacy-tgt"
                                :subject "subj"
                                :object "obj"
                                :predicate "employed-by")))
    (is (equal (relation-subject relation) "subj"))
    (is (equal (relation-object relation) "obj"))
    (is (equal (relation-source relation) "legacy-src"))
    (is (equal (relation-target relation) "legacy-tgt"))
    (is (equal (relation-predicate relation) "employed-by"))))

(test new-relation-subject-defaults-from-source
  "new-relation without :subject uses :source as subject"
  (let ((relation (new-relation "ds" "src" "tgt")))
    (is (equal (relation-subject relation) "src"))
    (is (equal (relation-object relation) "tgt"))
    (is (equal (relation-source relation) "src"))
    (is (equal (relation-target relation) "tgt"))))

(test relation-encode-emits-subject-object-and-legacy-aliases
  "v0.9 encode emits subject, object, source, and target in data"
  (let* ((relation (new-relation "ds" "src-id" "tgt-id"
                                 :predicate "employed-by"))
         (encoded (starintel:encode-document-v09 relation))
         (data (jsown:val encoded "data")))
    (is (equal (jsown:val data "subject") "src-id"))
    (is (equal (jsown:val data "object") "tgt-id"))
    (is (equal (jsown:val data "source") "src-id"))
    (is (equal (jsown:val data "target") "tgt-id"))
    (is (equal (jsown:val data "predicate") "employed-by"))))

(test digest-id-is-deterministic
  "digest-id returns the same SHA-256 hex string for the same inputs"
  (let ((id-1 (starintel:digest-id "alpha" "beta" "gamma"))
        (id-2 (starintel:digest-id "alpha" "beta" "gamma")))
    (is (stringp id-1))
    (is (= (length id-1) 64))
    (is (equal id-1 id-2))))

(test digest-id-distinguishes-inputs
  "digest-id differs for different inputs"
  (is (not (equal (starintel:digest-id "a" "b")
                  (starintel:digest-id "b" "a")))))
