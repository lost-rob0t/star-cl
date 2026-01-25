(in-package :starintel)

(defvar +relation/related-to+ "related-to")

;;; Identity / equivalence
(defvar +relation/same-as+ "same-as")
(defvar +relation/duplicate-of+ "duplicate-of")
(defvar +relation/aka+ "aka")
(defvar +relation/alias-of+ "alias-of")
(defvar +relation/username-of+ "username-of")
(defvar +relation/email-of+ "email-of")
(defvar +relation/phone-of+ "phone-of")
(defvar +relation/account-of+ "account-of")

;;; People / org / social structure
(defvar +relation/member-of+ "member-of")
(defvar +relation/employed-by+ "employed-by")
(defvar +relation/contractor-for+ "contractor-for")
(defvar +relation/works-with+ "works-with")
(defvar +relation/manages+ "manages")
(defvar +relation/reports-to+ "reports-to")

;;; Ownership / control / operation
(defvar +relation/owns+ "owns")
(defvar +relation/owned-by+ "owned-by")
(defvar +relation/controls+ "controls")
(defvar +relation/controlled-by+ "controlled-by")
(defvar +relation/operates+ "operates")
(defvar +relation/operated-by+ "operated-by")
(defvar +relation/administers+ "administers")
(defvar +relation/administered-by+ "administered-by")

;;; Registration / WHOIS-ish (OSINT classic)
(defvar +relation/registered-to+ "registered-to")
(defvar +relation/registrant-of+ "registrant-of")
(defvar +relation/whois-registrant-of+ "whois-registrant-of")
(defvar +relation/whois-admin-of+ "whois-admin-of")
(defvar +relation/whois-tech-of+ "whois-tech-of")

;;; Location / geo
(defvar +relation/located-at+ "located-at")
(defvar +relation/geolocated-at+ "geolocated-at")
(defvar +relation/seen-at+ "seen-at")

;;; Communications / interaction
(defvar +relation/communicates-with+ "communicates-with")
(defvar +relation/contacted+ "contacted")
(defvar +relation/contacted-by+ "contacted-by")
(defvar +relation/mentions+ "mentions")
(defvar +relation/replies-to+ "replies-to")
(defvar +relation/follows+ "follows")

;;; Web / URL relations
(defvar +relation/links-to+ "links-to")
(defvar +relation/redirects-to+ "redirects-to")
(defvar +relation/canonical-url-of+ "canonical-url-of")
(defvar +relation/hosts+ "hosts")
(defvar +relation/hosted-by+ "hosted-by")
(defvar +relation/served-by+ "served-by")

;;; DNS / infra (bug bounty + OSINT bread & butter)
(defvar +relation/resolves-to+ "resolves-to")
(defvar +relation/ptr-to+ "ptr-to")
(defvar +relation/has-a+ "has-a")
(defvar +relation/has-aaaa+ "has-aaaa")
(defvar +relation/has-cname+ "has-cname")
(defvar +relation/has-ns+ "has-ns")
(defvar +relation/has-mx+ "has-mx")
(defvar +relation/has-txt+ "has-txt")
(defvar +relation/has-spf+ "has-spf")
(defvar +relation/has-dkim+ "has-dkim")
(defvar +relation/has-dmarc+ "has-dmarc")
(defvar +relation/has-soa+ "has-soa")
(defvar +relation/behind-cdn+ "behind-cdn")

;;; Network / hosting
(defvar +relation/belongs-to-asn+ "belongs-to-asn")
(defvar +relation/served-from+ "served-from")
(defvar +relation/shares-ip-with+ "shares-ip-with")
(defvar +relation/shares-asn-with+ "shares-asn-with")

;;; Services / ports (bbp recon core)
(defvar +relation/hosts-service+ "hosts-service")
(defvar +relation/listens-on+ "listens-on")
(defvar +relation/exposes-port+ "exposes-port")
(defvar +relation/runs+ "runs")
(defvar +relation/runs-on+ "runs-on")

;;; Breach / credentialing
(defvar +relation/leaked-in+ "leaked-in")
(defvar +relation/credential-for+ "credential-for")
(defvar +relation/compromised-by+ "compromised-by")

;;; Forensics / provenance / evidence chain
(defvar +relation/observed-on+ "observed-on")
(defvar +relation/observed-by+ "observed-by")
(defvar +relation/collected-from+ "collected-from")
(defvar +relation/extracted-from+ "extracted-from")
(defvar +relation/derived-from+ "derived-from")
(defvar +relation/downloaded-from+ "downloaded-from")
(defvar +relation/uploaded-to+ "uploaded-to")
(defvar +relation/created-by+ "created-by")
(defvar +relation/modified-by+ "modified-by")
(defvar +relation/hashes-to+ "hashes-to")
(defvar +relation/matches-hash+ "matches-hash")
(defvar +relation/evidence-of+ "evidence-of")
(defvar +relation/indicates+ "indicates")

(defvar +relation/attributed-to+ "attributed-to")
(defvar +relation/uses+ "uses")
(defvar +relation/targets+ "targets")
(defvar +relation/exploits+ "exploits")
(defvar +relation/mitigates+ "mitigates")
(defvar +relation/c2-for+ "c2-for")

(defvar +relation/in-scope-of+ "in-scope-of")
(defvar +relation/out-of-scope-of+ "out-of-scope-of")
(defvar +relation/discovered-by+ "discovered-by")
(defvar +relation/scanned-by+ "scanned-by")
(defvar +relation/has-finding+ "has-finding")
(defvar +relation/vulnerable-to+ "vulnerable-to")

(defvar +relation-allowlist+
  (list
   +relation/related-to+
   +relation/same-as+ +relation/duplicate-of+ +relation/aka+
   +relation/alias-of+ +relation/username-of+ +relation/email-of+
   +relation/phone-of+ +relation/account-of+

   +relation/member-of+ +relation/employed-by+ +relation/contractor-for+
   +relation/works-with+ +relation/manages+ +relation/reports-to+

   +relation/owns+ +relation/owned-by+ +relation/controls+
   +relation/controlled-by+ +relation/operates+ +relation/operated-by+
   +relation/administers+ +relation/administered-by+

   +relation/registered-to+ +relation/registrant-of+
   +relation/whois-registrant-of+ +relation/whois-admin-of+ +relation/whois-tech-of+

   +relation/located-at+ +relation/geolocated-at+ +relation/seen-at+

   +relation/communicates-with+ +relation/contacted+ +relation/contacted-by+
   +relation/mentions+ +relation/replies-to+ +relation/follows+

   +relation/links-to+ +relation/redirects-to+ +relation/canonical-url-of+
   +relation/hosts+ +relation/hosted-by+ +relation/served-by+

   +relation/resolves-to+ +relation/ptr-to+
   +relation/has-a+ +relation/has-aaaa+ +relation/has-cname+
   +relation/has-ns+ +relation/has-mx+ +relation/has-txt+
   +relation/has-spf+ +relation/has-dkim+ +relation/has-dmarc+ +relation/has-soa+
   +relation/behind-cdn+

   +relation/belongs-to-asn+ +relation/served-from+
   +relation/shares-ip-with+ +relation/shares-asn-with+

   +relation/hosts-service+ +relation/listens-on+ +relation/exposes-port+
   +relation/runs+ +relation/runs-on+

   +relation/leaked-in+ +relation/credential-for+ +relation/compromised-by+

   +relation/observed-on+ +relation/observed-by+
   +relation/collected-from+ +relation/extracted-from+ +relation/derived-from+
   +relation/downloaded-from+ +relation/uploaded-to+
   +relation/created-by+ +relation/modified-by+
   +relation/hashes-to+ +relation/matches-hash+
   +relation/evidence-of+ +relation/indicates+

   +relation/attributed-to+ +relation/uses+ +relation/targets+
   +relation/exploits+ +relation/mitigates+ +relation/c2-for+

   +relation/in-scope-of+ +relation/out-of-scope-of+
   +relation/discovered-by+ +relation/scanned-by+
   +relation/has-finding+ +relation/vulnerable-to+))

(defun relation-valid-p (predicate)
  (and (stringp predicate)
       (> (length predicate) 0)
       (member predicate +relation-allowlist+ :test #'string=)))

(defun relation/assert-predicate (predicate)
  (unless (relation-valid-p predicate)
    (error "Invalid relation predicate: ~s. Allowed: ~{~a~^, ~}"
           predicate +relation-allowlist+))
  predicate)


(defclass relation (document)
  ((source :accessor relation-source :type string :initarg :source :initform "")
   (target :accessor relation-target :type string :initarg :target :initform "")
   (predicate :accessor relation-predicate :type string :initarg :predicate :initform +relation/related-to+)
   (note :accessor relation-note :type string :initarg :note :initform "")))

(defmethod set-id ((doc relation))
  (ulid-id doc))

(defun new-relation (dataset source target &key note (predicate +relation/related-to+))
  "Create a new StarIntel Relation (directed, labeled edge)."
  (relation/assert-predicate predicate)
  (let ((relation (make-instance 'relation
                                 :dataset dataset
                                 :source source
                                 :target target
                                 :predicate predicate
                                 :note (or note ""))))
    (set-meta relation dataset)
    relation))
