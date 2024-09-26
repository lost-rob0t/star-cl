(in-package :cl-user)
(uiop:define-package :starintel
  (:nicknames :spec)
  (:import-from :alexandria :flatten)
  (:use :cl)
  (:export
   #:add-inscope
   #:address
   #:address-city
   #:address-country
   #:address-postal
   #:address-state
   #:address-street
   #:address-street2
   #:add-scope
   #:asn
   #:asn-number
   #:asn-subnet
   #:breach
   #:breach-description
   #:breach-total
   #:breach-url
   #:doc-added
   #:doc-dataset
   #:doc-eid
   #:doc-etype
   #:doc-id
   #:doc-sources
   #:doc-type
   #:document
   #:doc-updated
   #:domain
   #:domain-resolved
   #:email
   #:email-domain
   #:email-message
   #:email-message-bcc
   #:email-message-body
   #:email-message-cc
   #:email-message-from
   #:email-message-headers
   #:email-message-subject
   #:email-message-to
   #:email-password
   #:email-user
   #:geo
   #:geo-alt
   #:geo-lat
   #:geo-long
   #:hash-id
   #:host
   #:host-hostname
   #:host-ip
   #:host-os
   #:host-ports
   #:make-instance
   #:make-ulid
   #:message
   #:message-channel
   #:message-content
   #:message-group
   #:message-is-reply
   #:message-media
   #:message-mentions
   #:message-message-id
   #:message-platform
   #:message-reply-to
   #:message-user
   #:name
   #:network
   #:network-asn
   #:network-org
   #:new-address
   #:new-asn
   #:new-domain
   #:new-email
   #:new-email-with-password
   #:new-host
   #:new-message
   #:new-network
   #:new-org
   #:new-phone
   #:new-port
   #:new-relation
   #:new-social-media-post
   #:new-target
   #:new-target-without-options
   #:new-url
   #:new-user
   #:new-username
   #:org
   #:org-bio
   #:org-country
   #:org-name
   #:org-reg
   #:org-website
   #:person
   #:person-fname
   #:person-lname
   #:person-mname
   #:phone
   #:phone-carrier
   #:phone-number
   #:phone-status
   #:phone-type
   #:relation
   #:relation-note
   #:relation-source
   #:relation-target
   #:scope
   #:scope-add-to-options
   #:scope-description
   #:scope-in
   #:scope-out
   #:service
   #:service-port
   #:set-id
   #:set-meta
   #:set-type
   #:social-media-post-content
   #:social-media-post-group
   #:social-media-post-links
   #:social-media-post-media
   #:social-media-post-replies
   #:social-media-post-reply-count
   #:social-media-post-reply-to
   #:social-media-post-repost-count
   #:social-media-post-tags
   #:social-media-post-title
   #:social-media-post-url
   #:social-media-post-user
   #:socialmpost
   #:target
   #:target-actor
   #:target-dataset
   #:target-id
   #:target-options
   #:target-target
   #:timestamp
   #:ulid-id
   #:unix-now
   #:update-timetamp
   #:url
   #:url-content
   #:url-path
   #:url-uri
   #:url-url
   #:user
   #:user-bio
   #:user-misc
   #:user-name
   #:user-platform
   #:user-url
   #:version
   #:web
   #:web-source))
