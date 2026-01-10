(in-package :starintel)

;;;; JSON encoding/decoding using Shasht
;;;; This file provides methods for serializing and deserializing
;;;; starintel document types to/from JSON

;;; Base document encoding
(defmethod shasht:print-json-value ((doc document) output-stream)
  "Base encoding method for all document types"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)))

;;; Person encoding
(defmethod shasht:print-json-value ((doc person) output-stream)
  "Encoding method for person documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "fname" (person-fname doc) output-stream)
    (shasht:print-json-key-value "mname" (person-mname doc) output-stream)
    (shasht:print-json-key-value "lname" (person-lname doc) output-stream)
    (shasht:print-json-key-value "bio" (slot-value doc 'bio) output-stream)
    (shasht:print-json-key-value "dob" (slot-value doc 'dob) output-stream)
    (shasht:print-json-key-value "region" (slot-value doc 'region) output-stream)
    (shasht:print-json-key-value "misc" (slot-value doc 'misc) output-stream)
    (shasht:print-json-key-value "etype" (doc-etype doc) output-stream)
    (shasht:print-json-key-value "eid" (doc-eid doc) output-stream)))

;;; Organization encoding
(defmethod shasht:print-json-value ((doc org) output-stream)
  "Encoding method for org documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "reg" (org-reg doc) output-stream)
    (shasht:print-json-key-value "name" (org-name doc) output-stream)
    (shasht:print-json-key-value "bio" (org-bio doc) output-stream)
    (shasht:print-json-key-value "country" (org-country doc) output-stream)
    (shasht:print-json-key-value "website" (org-website doc) output-stream)
    (shasht:print-json-key-value "etype" (doc-etype doc) output-stream)
    (shasht:print-json-key-value "eid" (doc-eid doc) output-stream)))

;;; Domain encoding
(defmethod shasht:print-json-value ((doc domain) output-stream)
  "Encoding method for domain documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "record_type" (domain-record-type doc) output-stream)
    (shasht:print-json-key-value "record" (domain-record doc) output-stream)
    (shasht:print-json-key-value "resolved_addresses" (domain-resolved doc) output-stream)))

;;; Network encoding
(defmethod shasht:print-json-value ((doc network) output-stream)
  "Encoding method for network documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "org" (network-org doc) output-stream)
    (shasht:print-json-key-value "subnet" (slot-value doc 'subnet) output-stream)
    (shasht:print-json-key-value "asn" (network-asn doc) output-stream)))

;;; Host encoding
(defmethod shasht:print-json-value ((doc host) output-stream)
  "Encoding method for host documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "hostname" (host-hostname doc) output-stream)
    (shasht:print-json-key-value "ip" (host-ip doc) output-stream)
    (shasht:print-json-key-value "os" (slot-value doc 'os) output-stream)
    (shasht:print-json-key-value "ports" (slot-value doc 'ports) output-stream)))

;;; URL encoding
(defmethod shasht:print-json-value ((doc url) output-stream)
  "Encoding method for url documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "url" (url-uri doc) output-stream)
    (shasht:print-json-key-value "path" (url-path doc) output-stream)
    (shasht:print-json-key-value "content" (slot-value doc 'content) output-stream)))

;;; Breach encoding
(defmethod shasht:print-json-value ((doc breach) output-stream)
  "Encoding method for breach documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "total" (breach-total doc) output-stream)
    (shasht:print-json-key-value "description" (breach-description doc) output-stream)
    (shasht:print-json-key-value "url" (breach-url doc) output-stream)))

;;; Email encoding
(defmethod shasht:print-json-value ((doc email) output-stream)
  "Encoding method for email documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "user" (email-user doc) output-stream)
    (shasht:print-json-key-value "domain" (email-domain doc) output-stream)
    (shasht:print-json-key-value "password" (email-password doc) output-stream)))

;;; Email-message encoding
(defmethod shasht:print-json-value ((doc email-message) output-stream)
  "Encoding method for email-message documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "body" (email-message-body doc) output-stream)
    (shasht:print-json-key-value "subject" (email-message-subject doc) output-stream)
    (shasht:print-json-key-value "to" (email-message-to doc) output-stream)
    (shasht:print-json-key-value "from" (email-message-from doc) output-stream)
    (shasht:print-json-key-value "headers" (email-message-headers doc) output-stream)
    (shasht:print-json-key-value "cc" (email-message-cc doc) output-stream)
    (shasht:print-json-key-value "bcc" (email-message-bcc doc) output-stream)))

;;; User encoding
(defmethod shasht:print-json-value ((doc user) output-stream)
  "Encoding method for user documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "url" (user-url doc) output-stream)
    (shasht:print-json-key-value "name" (user-name doc) output-stream)
    (shasht:print-json-key-value "platform" (user-platform doc) output-stream)
    (shasht:print-json-key-value "misc" (user-misc doc) output-stream)
    (shasht:print-json-key-value "bio" (user-bio doc) output-stream)))

;;; Relation encoding
(defmethod shasht:print-json-value ((doc relation) output-stream)
  "Encoding method for relation documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "source" (relation-source doc) output-stream)
    (shasht:print-json-key-value "target" (relation-target doc) output-stream)
    (shasht:print-json-key-value "note" (relation-note doc) output-stream)))

;;; Target encoding
(defmethod shasht:print-json-value ((doc target) output-stream)
  "Encoding method for target documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "actor" (target-actor doc) output-stream)
    (shasht:print-json-key-value "target" (target-target doc) output-stream)
    (shasht:print-json-key-value "delay" (target-delay doc) output-stream)
    (shasht:print-json-key-value "recurring" (target-recurring-p doc) output-stream)
    (shasht:print-json-key-value "options" (target-options doc) output-stream)))

;;; Message encoding
(defmethod shasht:print-json-value ((doc message) output-stream)
  "Encoding method for message documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "message" (message-content doc) output-stream)
    (shasht:print-json-key-value "platform" (message-platform doc) output-stream)
    (shasht:print-json-key-value "user" (message-user doc) output-stream)
    (shasht:print-json-key-value "is_reply" (message-is-reply doc) output-stream)
    (shasht:print-json-key-value "media" (message-media doc) output-stream)
    (shasht:print-json-key-value "message_id" (slot-value doc 'message-id) output-stream)
    (shasht:print-json-key-value "reply_to" (message-reply-to doc) output-stream)
    (shasht:print-json-key-value "group" (message-group doc) output-stream)
    (shasht:print-json-key-value "channel" (message-channel doc) output-stream)
    (shasht:print-json-key-value "mentions" (message-mentions doc) output-stream)))

;;; Social media post encoding
(defmethod shasht:print-json-value ((doc socialmpost) output-stream)
  "Encoding method for socialmpost documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "content" (social-media-post-content doc) output-stream)
    (shasht:print-json-key-value "user" (social-media-post-user doc) output-stream)
    (shasht:print-json-key-value "replies" (social-media-post-replies doc) output-stream)
    (shasht:print-json-key-value "media" (social-media-post-media doc) output-stream)
    (shasht:print-json-key-value "reply_count" (social-media-post-reply-count doc) output-stream)
    (shasht:print-json-key-value "repost_count" (social-media-post-repost-count doc) output-stream)
    (shasht:print-json-key-value "url" (social-media-post-url doc) output-stream)
    (shasht:print-json-key-value "links" (social-media-post-links doc) output-stream)
    (shasht:print-json-key-value "tags" (social-media-post-tags doc) output-stream)
    (shasht:print-json-key-value "title" (social-media-post-title doc) output-stream)
    (shasht:print-json-key-value "group" (social-media-post-group doc) output-stream)
    (shasht:print-json-key-value "reply_to" (social-media-post-reply-to doc) output-stream)))

;;; Geo encoding
(defmethod shasht:print-json-value ((doc geo) output-stream)
  "Encoding method for geo documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "lat" (geo-lat doc) output-stream)
    (shasht:print-json-key-value "long" (geo-long doc) output-stream)
    (shasht:print-json-key-value "alt" (geo-alt doc) output-stream)))

;;; Address encoding
(defmethod shasht:print-json-value ((doc address) output-stream)
  "Encoding method for address documents"
  (shasht:with-json-object output-stream
    (shasht:print-json-key-value "_id" (doc-id doc) output-stream)
    (shasht:print-json-key-value "dataset" (doc-dataset doc) output-stream)
    (shasht:print-json-key-value "dtype" (doc-type doc) output-stream)
    (shasht:print-json-key-value "sources" (doc-sources doc) output-stream)
    (shasht:print-json-key-value "version" (doc-version doc) output-stream)
    (shasht:print-json-key-value "date_updated" (doc-updated doc) output-stream)
    (shasht:print-json-key-value "date_added" (doc-added doc) output-stream)
    (shasht:print-json-key-value "lat" (geo-lat doc) output-stream)
    (shasht:print-json-key-value "long" (geo-long doc) output-stream)
    (shasht:print-json-key-value "alt" (geo-alt doc) output-stream)
    (shasht:print-json-key-value "city" (address-city doc) output-stream)
    (shasht:print-json-key-value "state" (address-state doc) output-stream)
    (shasht:print-json-key-value "postal" (address-postal doc) output-stream)
    (shasht:print-json-key-value "country" (address-country doc) output-stream)
    (shasht:print-json-key-value "street" (address-street doc) output-stream)
    (shasht:print-json-key-value "street2" (slot-value doc 'street2) output-stream)))

;;; Convenience functions for encoding documents to JSON
(defun encode (doc &key (pretty nil))
  "Encode a document to a JSON string"
  (shasht:write-json* doc :stream nil :pretty pretty))

(defun encode-to-stream (doc stream &key (pretty nil))
  "Encode a document to a JSON stream"
  (shasht:write-json* doc :stream stream :pretty pretty))
