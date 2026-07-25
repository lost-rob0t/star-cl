# StarIntel Common Lisp v0.9

The Common Lisp runtime retains the existing CLOS subtype classes while encoding canonical StarIntel v0.9 wire documents.

The document encoder writes snake_case envelope fields, ISO-8601 timestamps, integer record versions, structured sources, the declared `schema_org` JSON-LD block, and nests subtype slots under `data`.

```lisp
(let* ((organization (make-instance 'starintel:org
                                    :dataset "example"
                                    :name "Example Org"
                                    :reg "123"
                                    :country "US"))
       (wire (starintel:encode organization)))
  (jsown:val wire "schema_version")
  (jsown:val (jsown:val wire "data") "name")
  (jsown:val (jsown:val wire "schema_org") "@type"))
```

The dtype-to-Schema.org map covers all 49 canonical document types and is checked for drift at load time.
