# StarIntel Common Lisp v0.9

The Common Lisp runtime retains the existing CLOS subtype classes while encoding canonical StarIntel v0.9 wire documents.

The document encoder writes snake_case envelope fields, ISO-8601 timestamps, integer record versions, structured sources, the declared `schema_org` JSON-LD block, and nests subtype slots under `data`.

`starintel:encode` returns a JSOWN object. Serialization is explicit:

```lisp
(let* ((organization (make-instance 'starintel:org
                                    :dataset "example"
                                    :name "Example Org"
                                    :reg "123"
                                    :country "US"))
       (wire (starintel:encode organization))
       (json (jsown:to-json wire)))
  (declare (ignore json))
  (jsown:val wire "schema_version")
  (jsown:val (jsown:val wire "data") "name")
  (jsown:val (jsown:val wire "schema_org") "@type"))
```

Encoding and decoding are type-directed. Boolean false, JSON null, empty arrays, empty strings, absent slots, and nested objects are handled separately. Invalid primitive values or collection elements signal `starintel:codec-validation-error` with the operation, path, expected type, offending value, and reason.

Document decoding uses the wire `dtype` and the registered dtype-to-CLOS-class map:

```lisp
(starintel:decode-document wire)
```

Passing a different document class to `starintel:decode` signals `starintel:document-class-mismatch`; an unknown dtype signals `starintel:unknown-document-dtype`. Dtype strings are never interned.

The dtype-to-Schema.org map covers all 49 canonical document types and is checked for drift at load time.
