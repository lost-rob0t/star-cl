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

(test domain-set-id-uses-hash
  "Test that domain set-id uses hash of record and type"
  (let ((domain (make-instance 'domain
                               :record-type "A"
                               :record "example.com")))
    (set-id domain)
    (is (stringp (doc-id domain)))
    (is (= (length (doc-id domain)) 32))))

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
                                :asn 12345)))
    (is (typep network 'network))
    (is (equal (network-org network) "Example Org"))
    (is (= (network-asn network) 12345))))

(test host-creation
  "Test host entity creation"
  (let ((host (make-instance 'host
                             :hostname "server.example.com"
                             :ip "192.168.1.1")))
    (is (typep host 'host))
    (is (equal (host-hostname host) "server.example.com"))
    (is (equal (host-ip host) "192.168.1.1"))))

(test host-set-id-uses-ip
  "Test that host set-id uses hash of IP address"
  (let ((host (make-instance 'host :ip "192.168.1.1")))
    (set-id host)
    (is (stringp (doc-id host)))
    (is (= (length (doc-id host)) 32))))

(test host-same-ip-same-id
  "Test that hosts with same IP get same ID"
  (let ((host1 (make-instance 'host :ip "192.168.1.1" :hostname "host1"))
        (host2 (make-instance 'host :ip "192.168.1.1" :hostname "host2")))
    (set-id host1)
    (set-id host2)
    (is (equal (doc-id host1) (doc-id host2)))))

(test url-creation
  "Test URL entity creation"
  (let ((url-obj (make-instance 'url
                                :url "https://example.com"
                                :path "/test")))
    (is (typep url-obj 'url))
    (is (equal (url-uri url-obj) "https://example.com"))
    (is (equal (url-path url-obj) "/test"))))

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
  (let ((url-obj (new-url "test-dataset"
                          :url "https://example.com"
                          :path "/test")))
    (is (typep url-obj 'url))
    (is (equal (doc-dataset url-obj) "test-dataset"))
    (is (equal (doc-type url-obj) "url"))
    (is (stringp (doc-id url-obj)))))
