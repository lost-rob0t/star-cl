(in-package :starintel-test)

(def-suite documents-test
  :description "Tests for document base class"
  :in starintel-test)

(in-suite documents-test)

(test unix-now-returns-integer
  "Test that unix-now returns a valid integer timestamp"
  (let ((now (unix-now)))
    (is (integerp now))
    (is (> now 0))))

(test document-creation
  "Test basic document creation"
  (let ((doc (make-instance 'document :dataset "test-dataset")))
    (is (typep doc 'document))
    (is (equal (doc-dataset doc) "test-dataset"))))

(test ulid-id-generation
  "Test ULID generation for documents"
  (let ((doc (make-instance 'document)))
    (ulid-id doc)
    (is (stringp (doc-id doc)))
    (is (> (length (doc-id doc)) 0))))

(test hash-id-generation
  "Test hash ID generation with data"
  (let ((doc (make-instance 'document)))
    (hash-id doc "test" "data")
    (is (stringp (doc-id doc)))
    (is (= (length (doc-id doc)) 32)))) ; MD5 hash is 32 chars

(test hash-id-deterministic
  "Test that hash-id produces same output for same input"
  (let ((doc1 (make-instance 'document))
        (doc2 (make-instance 'document)))
    (hash-id doc1 "test" "data")
    (hash-id doc2 "test" "data")
    (is (equal (doc-id doc1) (doc-id doc2)))))

(test timestamp-adds-dates
  "Test that timestamp sets date-added and date-updated"
  (let ((doc (make-instance 'document)))
    (setf (doc-added doc) nil)
    (setf (doc-updated doc) nil)
    (timestamp doc)
    (is (integerp (doc-added doc)))
    (is (integerp (doc-updated doc)))))

(test update-timestamp-changes-updated
  "Test that update-timestamp only changes date-updated"
  (let ((doc (make-instance 'document)))
    (timestamp doc)
    (let ((original-added (doc-added doc)))
      (sleep 0.1)
      (update-timetamp doc)
      (is (= original-added (doc-added doc)))
      (is (>= (doc-updated doc) original-added)))))

(test set-type-extracts-class-name
  "Test that set-type correctly sets document type"
  (let ((doc (make-instance 'document)))
    (set-type doc)
    (is (equal (doc-type doc) "document"))))

(test set-meta-sets-all-metadata
  "Test that set-meta sets dataset and type for person (subclass with set-id)"
  (let ((person (make-instance 'person)))
    (set-meta person "test-dataset")
    (is (equal (doc-dataset person) "test-dataset"))
    (is (equal (doc-type person) "person"))
    (is (stringp (doc-id person)))
    (is (> (length (doc-id person)) 0))))

(test set-meta-doesnt-overwrite-existing-id
  "Test that set-meta doesn't overwrite an existing ID"
  (let ((org (make-instance 'org :name "Test" :reg "123" :country "US")))
    (hash-id org "existing" "id")
    (let ((original-id (doc-id org)))
      (set-meta org "test-dataset")
      (is (equal (doc-id org) original-id)))))

(test encoder-excludes-unset-rev
  "Test that encoder excludes _rev when not set"
  (let* ((doc (make-instance 'document))
         (json (jsown:to-json (encode doc))))
    (is (not (search "_rev" json)))))

(test encoder-includes-set-rev
  "Test that encoder includes _rev when set"
  (let* ((doc (make-instance 'document :rev "1-abc123"))
         (json (jsown:to-json (encode doc))))
    (is (search "_rev" json))
    (is (search "1-abc123" json))))
