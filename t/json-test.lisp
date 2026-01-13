(in-package :starintel-test)

(def-suite json-test
  :description "Tests for JSON encoding/decoding"
  :in starintel-test)

(in-suite json-test)

(test encode-returns-string
  "Test that encode returns a string"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (json (encode person)))
    (is (stringp json))
    (is (> (length json) 0))))

(test encode-person-has-required-fields
  "Test that encoded person contains required fields"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"
                             :mname "Q"
                             :bio "Test bio"))
         (json (encode person)))
    (is (search "\"_id\":" json))
    (is (search "\"dataset\":" json))
    (is (search "\"dtype\":" json))
    (is (search "\"fname\":\"John\"" json))
    (is (search "\"lname\":\"Doe\"" json))
    (is (search "\"mname\":\"Q\"" json))
    (is (search "\"bio\":\"Test bio\"" json))))

(test encode-org-has-required-fields
  "Test that encoded org contains required fields"
  (let* ((org (new-org "test-dataset" "ACME Corp" "org"
                       :country "US"
                       :reg "12345"))
         (json (encode org)))
    (is (search "\"_id\":" json))
    (is (search "\"dataset\":" json))
    (is (search "\"name\":\"ACME Corp\"" json))
    (is (search "\"country\":\"US\"" json))
    (is (search "\"reg\":\"12345\"" json))))

(test encode-domain-has-required-fields
  "Test that encoded domain contains required fields"
  (let* ((domain (new-domain "test-dataset"
                             :record "example.com"
                             :record-type "A"))
         (json (encode domain)))
    (is (search "\"_id\":" json))
    (is (search "\"record\":\"example.com\"" json))
    (is (search "\"record_type\":\"A\"" json))))

(test encode-network-has-required-fields
  "Test that encoded network contains required fields"
  (let* ((network (new-network "test-dataset"
                               :org "Test Org"
                               :subnet "192.168.1.0/24"
                               :asn 12345))
         (json (encode network)))
    (is (search "\"_id\":" json))
    (is (search "\"org\":\"Test Org\"" json))
    (is (search "\"subnet\":\"192.168.1.0/24\"" json))
    (is (search "\"asn\":12345" json))))

(test encode-host-has-required-fields
  "Test that encoded host contains required fields"
  (let* ((host (new-host "test-dataset"
                         :hostname "server.example.com"
                         :ip "192.168.1.100"))
         (json (encode host)))
    (is (search "\"_id\":" json))
    (is (search "\"hostname\":\"server.example.com\"" json))
    (is (search "\"ip\":\"192.168.1.100\"" json))))

(test encode-url-has-required-fields
  "Test that encoded url contains required fields"
  (let* ((url (new-url "test-dataset"
                       :url "https://example.com"
                       :path "/test"))
         (json (encode url)))
    (is (search "\"_id\":" json))
    (is (search "\"url\":\"https://example.com\"" json))
    (is (search "\"path\":\"/test\"" json))))

(test encode-email-has-required-fields
  "Test that encoded email contains required fields"
  (let* ((email (new-email* "test-dataset" "user@example.com"
                            :password "secret123"))
         (json (encode email)))
    (is (search "\"_id\":" json))
    (is (search "\"user\":\"user\"" json))
    (is (search "\"domain\":\"example.com\"" json))
    (is (search "\"password\":\"secret123\"" json))))

(test encode-user-has-required-fields
  "Test that encoded user contains required fields"
  (let* ((user (new-user "test-dataset"
                         :name "testuser"
                         :platform "twitter"
                         :url "https://twitter.com/testuser"))
         (json (encode user)))
    (is (search "\"_id\":" json))
    (is (search "\"name\":\"testuser\"" json))
    (is (search "\"platform\":\"twitter\"" json))
    (is (search "\"url\":\"https://twitter.com/testuser\"" json))))

(test encode-relation-has-required-fields
  "Test that encoded relation contains required fields"
  (let* ((relation (new-relation "test-dataset"
                                 "source-id"
                                 "target-id"
                                 "test note"))
         (json (encode relation)))
    (is (search "\"_id\":" json))
    (is (search "\"source\":\"source-id\"" json))
    (is (search "\"target\":\"target-id\"" json))
    (is (search "\"note\":\"test note\"" json))))

(test encode-target-has-required-fields
  "Test that encoded target contains required fields"
  (let* ((target (new-target "test-dataset"
                             "target-value"
                             "actor-name"
                             :delay 60
                             :recurring t))
         (json (encode target)))
    (is (search "\"_id\":" json))
    (is (search "\"target\":\"target-value\"" json))
    (is (search "\"actor\":\"actor-name\"" json))
    (is (search "\"delay\":60" json))
    (is (search "\"recurring\":true" json))))

(test encode-message-has-required-fields
  "Test that encoded message contains required fields"
  (let* ((message (new-message "test-dataset"
                               :content "Hello world"
                               :platform "telegram"
                               :user "testuser"
                               :channel "general"))
         (json (encode message)))
    (is (search "\"_id\":" json))
    (is (search "\"message\":\"Hello world\"" json))
    (is (search "\"platform\":\"telegram\"" json))
    (is (search "\"user\":\"testuser\"" json))
    (is (search "\"channel\":\"general\"" json))))

(test encode-social-media-post-has-required-fields
  "Test that encoded social media post contains required fields"
  (let* ((post (new-social-media-post "test-dataset"
                                      :content "Test post"
                                      :user "testuser"
                                      :url "https://twitter.com/testuser/status/123"))
         (json (encode post)))
    (is (search "\"_id\":" json))
    (is (search "\"content\":\"Test post\"" json))
    (is (search "\"user\":\"testuser\"" json))
    (is (search "\"url\":\"https://twitter.com/testuser/status/123\"" json))))

(test encode-geo-has-required-fields
  "Test that encoded geo contains required fields"
  (let* ((geo (new-geo "test-dataset"
                       :lat 40.7128
                       :long -74.0060
                       :alt 10.0))
         (json (encode geo)))
    (is (search "\"_id\":" json))
    (is (search "\"lat\":40.7128" json))
    (is (search "\"long\":-74.006" json))
    (is (search "\"alt\":10" json))))

(test encode-address-has-required-fields
  "Test that encoded address contains required fields"
  (let* ((address (new-address "test-dataset"
                               :city "New York"
                               :state "NY"
                               :postal "10001"
                               :country "US"
                               :street "123 Main St"))
         (json (encode address)))
    (is (search "\"_id\":" json))
    (is (search "\"city\":\"New York\"" json))
    (is (search "\"state\":\"NY\"" json))
    (is (search "\"postal\":\"10001\"" json))
    (is (search "\"country\":\"US\"" json))
    (is (search "\"street\":\"123 Main St\"" json))))

(test encode-pretty-adds-whitespace
  "Test that pretty encoding adds whitespace"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (json-compact (encode person))
         (json-pretty (encode person :pretty t)))
    (is (< (length json-compact) (length json-pretty)))
    (is (search (string #\Newline) json-pretty))))

(test encode-list-arrays-properly
  "Test that lists are encoded as JSON arrays"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"
                             :misc '("tag1" "tag2" "tag3")))
         (json (encode person)))
    (is (search "\"misc\":[\"tag1\",\"tag2\",\"tag3\"]" json))))

(test encode-empty-list-as-empty-array
  "Test that empty lists are encoded as empty JSON arrays"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (json (encode person)))
    (is (search "\"misc\":[]" json))))

(test encode-preserves-timestamps
  "Test that timestamps are preserved in encoding"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (json (encode person)))
    (is (search "\"date_added\":" json))
    (is (search "\"date_updated\":" json))))

(test encode-preserves-version
  "Test that version is preserved in encoding"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (json (encode person)))
    (is (search "\"version\":" json))
    (is (search +starintel-doc-version+ json))))

(test encode-preserves-sources
  "Test that sources list is preserved in encoding"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (json (encode person)))
    (setf (doc-sources person) '("source1" "source2"))
    (let ((json-with-sources (encode person)))
      (is (search "\"sources\":[\"source1\",\"source2\"]" json-with-sources)))))

(test encode-multiple-documents-independently
  "Test that encoding multiple documents works independently"
  (let* ((person1 (new-person "test-dataset" "John" "Doe" "person"))
         (person2 (new-person "test-dataset" "Jane" "Smith" "person"))
         (json1 (encode person1))
         (json2 (encode person2)))
    (is (search "\"fname\":\"John\"" json1))
    (is (search "\"fname\":\"Jane\"" json2))
    (is (not (search "Jane" json1)))
    (is (not (search "John" json2)))))

;;; Decoding tests

(test decode-person-basic-fields
  "Test that decoding person restores basic fields"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"
                             :mname "Q"
                             :bio "Test bio"))
         (json-obj (encode person))
         (decoded (decode json-obj 'person)))
    (is (equal (person-fname decoded) "John"))
    (is (equal (person-lname decoded) "Doe"))
    (is (equal (person-mname decoded) "Q"))
    (is (equal (doc-dataset decoded) "test-dataset"))
    (is (equal (doc-type decoded) "person"))))

(test decode-org-basic-fields
  "Test that decoding org restores basic fields"
  (let* ((org (new-org "test-dataset" "ACME Corp" "org"
                       :country "US"
                       :reg "12345"
                       :bio "Test company"))
         (json-obj (encode org))
         (decoded (decode json-obj 'org)))
    (is (equal (org-name decoded) "ACME Corp"))
    (is (equal (org-country decoded) "US"))
    (is (equal (org-reg decoded) "12345"))
    (is (equal (org-bio decoded) "Test company"))))

(test decode-domain-basic-fields
  "Test that decoding domain restores basic fields"
  (let* ((domain (new-domain "test-dataset"
                             :record "example.com"
                             :record-type "A"))
         (json-obj (encode domain))
         (decoded (decode json-obj 'domain)))
    (is (equal (domain-record decoded) "example.com"))
    (is (equal (domain-record-type decoded) "A"))))

(test decode-network-basic-fields
  "Test that decoding network restores basic fields"
  (let* ((network (new-network "test-dataset"
                               :org "Test Org"
                               :subnet "192.168.1.0/24"
                               :asn 12345))
         (json-obj (encode network))
         (decoded (decode json-obj 'network)))
    (is (equal (network-org decoded) "Test Org"))
    (is (equal (network-subnet decoded) "192.168.1.0/24"))
    (is (equal (network-asn decoded) 12345))))

(test decode-host-basic-fields
  "Test that decoding host restores basic fields"
  (let* ((host (new-host "test-dataset"
                         :hostname "server.example.com"
                         :ip "192.168.1.100"))
         (json-obj (encode host))
         (decoded (decode json-obj 'host)))
    (is (equal (host-hostname decoded) "server.example.com"))
    (is (equal (host-ip decoded) "192.168.1.100"))))

(test decode-url-basic-fields
  "Test that decoding url restores basic fields"
  (let* ((url (new-url "test-dataset"
                       :url "https://example.com"
                       :path "/test"))
         (json-obj (encode url))
         (decoded (decode json-obj 'url*)))
    (is (equal (url-url decoded) "https://example.com"))
    (is (equal (url-path decoded) "/test"))))

(test decode-email-basic-fields
  "Test that decoding email restores basic fields"
  (let* ((email (new-email* "test-dataset" "user@example.com"
                            :password "secret123"))
         (json-obj (encode email))
         (decoded (decode json-obj 'email*)))
    (is (equal (email-user decoded) "user"))
    (is (equal (email-domain decoded) "example.com"))
    (is (equal (email-password decoded) "secret123"))))

(test decode-user-basic-fields
  "Test that decoding user restores basic fields"
  (let* ((user (new-user "test-dataset"
                         :name "testuser"
                         :platform "twitter"
                         :url "https://twitter.com/testuser"))
         (json-obj (encode user))
         (decoded (decode json-obj 'user*)))
    (is (equal (user-name decoded) "testuser"))
    (is (equal (user-platform decoded) "twitter"))
    (is (equal (user-url decoded) "https://twitter.com/testuser"))))

(test decode-relation-basic-fields
  "Test that decoding relation restores basic fields"
  (let* ((relation (new-relation "test-dataset"
                                 "source-id"
                                 "target-id"
                                 "test note"))
         (json-obj (encode relation))
         (decoded (decode json-obj 'relation)))
    (is (equal (relation-source decoded) "source-id"))
    (is (equal (relation-target decoded) "target-id"))
    (is (equal (relation-note decoded) "test note"))))

(test decode-target-basic-fields
  "Test that decoding target restores basic fields"
  (let* ((target (new-target "test-dataset"
                             "target-value"
                             "actor-name"
                             :delay 60
                             :recurring t))
         (json-obj (encode target))
         (decoded (decode json-obj 'target)))
    (is (equal (target-target decoded) "target-value"))
    (is (equal (target-actor decoded) "actor-name"))
    (is (equal (target-delay decoded) 60))
    (is (eq (target-recurring decoded) t))))

(test decode-message-basic-fields
  "Test that decoding message restores basic fields"
  (let* ((message (new-message "test-dataset"
                               :content "Hello world"
                               :platform "telegram"
                               :user "testuser"
                               :channel "general"))
         (json-obj (encode message))
         (decoded (decode json-obj 'message)))
    (is (equal (message-content decoded) "Hello world"))
    (is (equal (message-platform decoded) "telegram"))
    (is (equal (message-user decoded) "testuser"))
    (is (equal (message-channel decoded) "general"))))

(test decode-social-media-post-basic-fields
  "Test that decoding social media post restores basic fields"
  (let* ((post (new-social-media-post "test-dataset"
                                      :content "Test post"
                                      :user "testuser"
                                      :url "https://twitter.com/testuser/status/123"))
         (json-obj (encode post))
         (decoded (decode json-obj 'social-media-post)))
    (is (equal (social-media-post-content decoded) "Test post"))
    (is (equal (social-media-post-user decoded) "testuser"))
    (is (equal (social-media-post-url decoded) "https://twitter.com/testuser/status/123"))))

(test decode-geo-basic-fields
  "Test that decoding geo restores basic fields"
  (let* ((geo (new-geo "test-dataset"
                       :lat 40.7128
                       :long -74.0060
                       :alt 10.0))
         (json-obj (encode geo))
         (decoded (decode json-obj 'geo)))
    (is (= (geo-lat decoded) 40.7128))
    (is (= (geo-long decoded) -74.0060))
    (is (= (geo-alt decoded) 10.0))))

(test decode-address-basic-fields
  "Test that decoding address restores basic fields"
  (let* ((address (new-address "test-dataset"
                               :city "New York"
                               :state "NY"
                               :postal "10001"
                               :country "US"
                               :street "123 Main St"))
         (json-obj (encode address))
         (decoded (decode json-obj 'address)))
    (is (equal (address-city decoded) "New York"))
    (is (equal (address-state decoded) "NY"))
    (is (equal (address-postal decoded) "10001"))
    (is (equal (address-country decoded) "US"))
    (is (equal (address-street decoded) "123 Main St"))))

(test decode-preserves-lists
  "Test that lists are properly decoded"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"
                             :misc '("tag1" "tag2" "tag3")))
         (json-obj (encode person))
         (decoded (decode json-obj 'person)))
    (is (listp (person-misc decoded)))
    (is (= (length (person-misc decoded)) 3))
    (is (member "tag1" (person-misc decoded) :test #'equal))
    (is (member "tag2" (person-misc decoded) :test #'equal))
    (is (member "tag3" (person-misc decoded) :test #'equal))))

(test decode-preserves-timestamps
  "Test that timestamps are preserved in decoding"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (original-added (doc-added person))
         (original-updated (doc-updated person))
         (json-obj (encode person))
         (decoded (decode json-obj 'person)))
    (is (= (doc-added decoded) original-added))
    (is (= (doc-updated decoded) original-updated))))

(test decode-preserves-id
  "Test that ID is preserved in decoding"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"))
         (original-id (doc-id person))
         (json-obj (encode person))
         (decoded (decode json-obj 'person)))
    (is (equal (doc-id decoded) original-id))))

(test decode-round-trip-equality
  "Test that encoding then decoding preserves all data"
  (let* ((person (new-person "test-dataset" "John" "Doe" "person"
                             :mname "Q"
                             :bio "Test bio"
                             :dob "1980-01-01"
                             :region "US"
                             :misc '("tag1" "tag2")))
         (json-obj (encode person))
         (decoded (decode json-obj 'person)))
    (is (equal (doc-id decoded) (doc-id person)))
    (is (equal (doc-dataset decoded) (doc-dataset person)))
    (is (equal (doc-type decoded) (doc-type person)))
    (is (equal (person-fname decoded) (person-fname person)))
    (is (equal (person-lname decoded) (person-lname person)))
    (is (equal (person-mname decoded) (person-mname person)))
    (is (= (doc-added decoded) (doc-added person)))
    (is (= (doc-updated decoded) (doc-updated person)))
    (is (equal (person-misc decoded) (person-misc person)))))
