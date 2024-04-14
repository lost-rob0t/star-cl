(in-package :cl-user)
(uiop:define-package :starintel
  (:nicknames :spec)
  (:import-from :alexandria :flatten)
  (:use :cl)
  (:export
   #:unix-now
   #:document
   #:doc-id
   #:doc-dataset
   #:doc-updated
   #:make-uuid
   #:timestamp
   #:update-timetamp
   #:hash-id
   #:set-type
   #:set-meta
   #:new-org
   #:org
   #:org-reg
   #:org-name
   #:org-bio
   #:org-country
   #:org-website
   #:doc-etype
   #:doc-eid
   #:person
   #:person-fname
   #:person-mname
   #:person-lname
   #:web
   #:web-source
   #:domain
   #:domain-record-type
   #:domain-record
   #:domain-ip
   #:port
   #:port-number
   #:port-services
   #:asn
   #:asn-number
   #:asn-subnet
   #:network
   #:network-org
   #:network-asn
   #:host
   #:host-hostname
   #:host-ip
   #:host-ports
   #:host-os
   #:new-domain
   #:new-port
   #:new-asn
   #:new-network
   #:make-instance
   #:new-url
   #:new-host
   #:address
   #:address-city
   #:address-state
   #:address-postal
   #:address-country
   #:address-street
   #:address-street2
   #:geo
   #:geo-lat
   #:geo-long
   #:geo-alt
   #:accessor
   #:new-address
   #:phone
   #:phone-number
   #:phone-carrier
   #:phone-status
   #:phone-type
   #:new-phone
   #:relation
   #:relation-source
   #:relation-target
   #:relation-note
   #:new-relation
   #:message
   #:message-message
   #:message-platform
   #:message-user
   #:message-is-reply
   #:message-media
   #:message-message-id
   #:message-reply-to
   #:message-group
   #:message-channel
   #:message-mentions
   #:social-media-post
   #:new-message
   #:new-social-media-post
   #:social-media-post-content
   #:social-media-post-user
   #:social-media-post-replies
   #:social-media-post-media
   #:social-media-post-reply-count
   #:social-media-post-repost-count
   #:social-media-post-url
   #:social-media-post-links
   #:social-media-post-tags
   #:social-media-post-title
   #:social-media-post-group
   #:social-media-post-reply-to
   #:target
   #:target-id
   #:target-actor
   #:target-dataset
   #:target-target
   #:target-options
   #:new-target
   #:new-target-without-options
   #:breach
   #:breach-total
   #:breach-description
   #:breach-url
   #:email
   #:email-user
   #:email-domain
   #:email-password
   #:email-message
   #:email-message-body
   #:email-message-subject
   #:email-message-to
   #:email-message-from
   #:email-message-headers
   #:email-message-cc
   #:email-message-bcc
   #:user
   #:user-url
   #:user-name
   #:user-platform
   #:user-misc
   #:user-bio
   #:new-email
   #:new-email-with-password
   #:new-user
   #:new-username
   #:doc-added
   #:doc-type))
