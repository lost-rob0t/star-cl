(in-package :starintel-test)

(def-suite hosts-test
  :description "Tests for host-related entities (domain, network, host, url)"
  :in starintel-test)

(in-suite hosts-test)

(test domain-creation
  "Test domain entity creation"
  (let ((domain (make-instance 'domain
                               :record-type "A"
                               :record "example.com")))
    (is (typep domain 'domain))
    (is (equal (domain-record-type domain) "A"))
    (is (equal (domain-record domain) "example.com"))))

(test domain-set-id-uses-sha256
  "Test that domain set-id uses SHA-256 over record and type"
  (let ((domain (make-instance 'domain
                               :record-type "A"
                               :record "example.com")))
    (set-id domain)
    (is (stringp (doc-id domain)))
    (is (= (length (doc-id domain)) 64))))

(test domain-hash-id-deterministic
  "Test that domain IDs are deterministic"
  (let ((domain1 (make-instance 'domain
                                :record-type "A"
                                :record "example.com"))
        (domain2 (make-instance 'domain
                                :record-type "A"
                                :record "example.com")))
    (set-id domain1)
    (set-id domain2)
    (is (equal (doc-id domain1) (doc-id domain2)))))

(test network-creation
  "Test network entity creation"
  (let ((network (make-instance 'network
                                :org "Example Org"
                                :subnet "192.168.1.0/24"
                                :asn 12345)))
    (is (typep network 'network))
    (is (equal (network-org network) "Example Org"))
    (is (equal (network-subnet network) "192.168.1.0/24"))
    (is (= (network-asn network) 12345))))

(test host-creation
  "Test host entity creation"
  (let ((host (make-instance 'host
                             :hostname "server.example.com"
                             :ip "192.168.1.1")))
    (is (typep host 'host))
    (is (equal (host-hostname host) "server.example.com"))
    (is (equal (host-ip host) "192.168.1.1"))))

(test host-set-id-uses-sha256
  "Test that resolved host set-id preserves the historical IP-only hash"
  (let ((host (make-instance 'host :ip "192.168.1.1")))
    (set-id host)
    (is (stringp (doc-id host)))
    (is (= (length (doc-id host)) 64))
    (is (equal (doc-id host) (digest-id "192.168.1.1")))))

(test host-same-ip-same-id
  "Test that resolved hosts with same IP retain the same historical ID"
  (let ((host1 (make-instance 'host :ip "192.168.1.1" :hostname "host1"))
        (host2 (make-instance 'host :ip "192.168.1.1" :hostname "host2")))
    (set-id host1)
    (set-id host2)
    (is (equal (doc-id host1) (doc-id host2)))
    (is (equal (doc-id host1) (digest-id "192.168.1.1")))))

(test unresolved-hostname-has-deterministic-distinct-id
  "Hostname-only hosts use tagged case-insensitive DNS identity"
  (let ((host1 (make-instance 'host :hostname "Pending.Example." :ip ""))
        (host2 (make-instance 'host :hostname "pending.example" :ip ""))
        (other (make-instance 'host :hostname "other.example" :ip "")))
    (set-id host1)
    (set-id host2)
    (set-id other)
    (is (equal (doc-id host1) (doc-id host2)))
    (is (equal (doc-id host1) (digest-id "hostname" "pending.example")))
    (is (not (equal (doc-id host1) (doc-id other))))))

(test host-without-ip-or-hostname-is-invalid
  "An empty host cannot mint a canonical document ID"
  (signals error
    (set-id (make-instance 'host :hostname "" :ip ""))))

(test new-unresolved-host-function
  "new-host can now create a hostname-only canonical document"
  (let ((host (new-host "test-dataset" :hostname "pending.example" :ip "")))
    (is (typep host 'host))
    (is (equal (doc-type host) "host"))
    (is (equal (doc-id host) (digest-id "hostname" "pending.example")))))

(test unresolved-host-v09-round-trip-preserves-identity
  "Hostname-only host identity survives canonical JSON encode/decode"
  (let* ((host (new-host "test-dataset"
                         :hostname "Pending.Example."
                         :ip ""))
         (expected-id (doc-id host))
         (decoded (decode-document (encode host))))
    (is (typep decoded 'host))
    (is (equal expected-id (doc-id decoded)))
    (is (equal "Pending.Example." (host-hostname decoded)))
    (is (equal "" (host-ip decoded)))))

(test url-creation
  "Test URL entity creation"
  (let ((url-object (make-instance 'url
                                   :url "https://example.com"
                                   :path "/test")))
    (is (typep url-object 'url))
    (is (equal (url-uri url-object) "https://example.com"))
    (is (equal (url-path url-object) "/test"))))

(test new-domain-function
  "Test new-domain convenience function"
  (let ((domain (new-domain "test-dataset"
                            :record-type "A"
                            :record "example.com")))
    (is (typep domain 'domain))
    (is (equal (doc-dataset domain) "test-dataset"))
    (is (equal (doc-type domain) "domain"))
    (is (stringp (doc-id domain)))))

(test new-network-function
  "Test new-network convenience function"
  (let ((network (new-network "test-dataset"
                              :org "Example Org"
                              :subnet "192.168.1.0/24"
                              :asn 12345)))
    (is (typep network 'network))
    (is (equal (doc-dataset network) "test-dataset"))
    (is (equal (doc-type network) "network"))
    (is (stringp (doc-id network)))))

(test new-host-function
  "Test new-host convenience function"
  (let ((host (new-host "test-dataset"
                        :hostname "server.example.com"
                        :ip "192.168.1.1")))
    (is (typep host 'host))
    (is (equal (doc-dataset host) "test-dataset"))
    (is (equal (doc-type host) "host"))
    (is (stringp (doc-id host)))))

(test new-url-function
  "Test new-url convenience function"
  (let ((url-object (new-url "test-dataset"
                             :url "https://example.com"
                             :path "/test")))
    (is (typep url-object 'url))
    (is (equal (doc-dataset url-object) "test-dataset"))
    (is (equal (doc-type url-object) "url"))
    (is (stringp (doc-id url-object)))))
