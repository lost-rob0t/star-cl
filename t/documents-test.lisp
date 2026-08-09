(in-package :starintel-test)

(def-suite documents-test
  :description "Tests for canonical v0.9 document base class"
  :in starintel-test)

(in-suite documents-test)

(test unix-now-returns-integer
  (let ((now (unix-now)))
    (is (integerp now))
    (is (> now 0))))

(test utc-now-returns-iso-string
  (let ((now (utc-now)))
    (is (stringp now))
    (is (search "T" now))))

(test document-creation
  (let ((doc (make-instance 'document :dataset "test-dataset")))
    (is (typep doc 'document))
    (is (equal (doc-dataset doc) "test-dataset"))
    (is (equal (doc-schema-version doc) "0.9.0"))
    (is (= (doc-version doc) 1))
    (is (stringp (doc-added doc)))
    (is (stringp (doc-updated doc)))))

(test ulid-id-generation
  (let ((doc (make-instance 'document)))
    (ulid-id doc)
    (is (stringp (doc-id doc)))
    (is (> (length (doc-id doc)) 0))))

(test hash-id-generation
  (let ((doc (make-instance 'document)))
    (hash-id doc "test" "data")
    (is (stringp (doc-id doc)))
    (is (= (length (doc-id doc)) 64))))

(test hash-id-deterministic
  (let ((doc1 (make-instance 'document))
        (doc2 (make-instance 'document)))
    (hash-id doc1 "test" "data")
    (hash-id doc2 "test" "data")
    (is (equal (doc-id doc1) (doc-id doc2)))))

(test timestamp-adds-iso-dates
  (let ((doc (make-instance 'document)))
    (setf (doc-added doc) ""
          (doc-updated doc) "")
    (timestamp doc)
    (is (stringp (doc-added doc)))
    (is (stringp (doc-updated doc)))
    (is (search "T" (doc-added doc)))))

(test update-timestamp-preserves-added
  (let ((doc (make-instance 'document)))
    (timestamp doc)
    (let ((original-added (doc-added doc)))
      (sleep 0.01)
      (update-timestamp doc)
      (is (string= original-added (doc-added doc)))
      (is (stringp (doc-updated doc))))))

(test set-type-extracts-canonical-class-name
  (let ((doc (make-instance 'document)))
    (set-type doc)
    (is (equal (doc-type doc) "document"))))

(test set-meta-sets-all-metadata
  (let ((person (make-instance 'person)))
    (set-meta person "test-dataset")
    (is (equal (doc-dataset person) "test-dataset"))
    (is (equal (doc-type person) "person"))
    (is (stringp (doc-id person)))
    (is (> (length (doc-id person)) 0))))

(test set-meta-doesnt-overwrite-existing-id
  (let ((org (make-instance 'org :name "Test" :reg "123" :country "US")))
    (hash-id org "existing" "id")
    (let ((original-id (doc-id org)))
      (set-meta org "test-dataset")
      (is (equal (doc-id org) original-id)))))

(test schema-org-covers-canonical-types
  (is (= (length +canonical-dtypes+) (length *dtype-schema-org-types*)))
  (is (equal (first (schema-org-types "org")) "Organization"))
  (is (equal (first (schema-org-types "social-media-post")) "SocialMediaPosting")))

(test encoder-emits-v09-envelope-and-nested-data
  (let* ((org (make-instance 'org
                             :dataset "test"
                             :name "Example Org"
                             :reg "123"
                             :country "US"
                             :sources '("https://example.test")))
         (encoded (encode org))
         (data (jsown:val encoded "data"))
         (schema-org (jsown:val encoded "schema_org"))
         (sources (jsown:val encoded "sources")))
    (is (string= (jsown:val encoded "schema_version") "0.9.0"))
    (is (= (jsown:val encoded "version") 1))
    (is (string= (jsown:val encoded "dtype") "org"))
    (is (string= (jsown:val data "name") "Example Org"))
    (is (string= (jsown:val schema-org "@context") "https://schema.org/"))
    (is (string= (jsown:val schema-org "@type") "Organization"))
    (is (string= (jsown:val (first sources) "url") "https://example.test"))))

(test encoder-excludes-unset-rev
  (let* ((doc (make-instance 'document))
         (json (jsown:to-json (encode doc))))
    (is (not (search "_rev" json)))))

(test encoder-includes-set-rev
  (let* ((doc (make-instance 'document :rev "1-abc123"))
         (json (jsown:to-json (encode doc))))
    (is (search "_rev" json))
    (is (search "1-abc123" json))))

(test encoder-excludes-invalid-rev
  (let* ((doc (make-instance 'document :rev "bogus"))
         (json (jsown:to-json (encode doc))))
    (is (not (search "_rev" json)))))
