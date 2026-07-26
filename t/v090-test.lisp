(in-package :starintel-test)

(def-suite v090-test
  :description "StarIntel v0.9.0 conformance runtime"
  :in starintel-test)

(in-suite v090-test)

(defun v090-test-document ()
  (com.inuoe.jzon:parse
   "{\"_id\":\"starintel:person:cl-test\",\"dataset\":\"test\",\"dtype\":\"person\",\"schema_version\":\"0.9.0\",\"version\":1,\"date_added\":\"2026-01-02T03:04:05Z\",\"date_updated\":\"2026-01-02T03:04:05+00:00\",\"sources\":[],\"evidence\":[],\"data\":{\"fname\":\"Ada\",\"lname\":\"Lovelace\",\"bio\":\"Unicode λ 漢字 🧠\"},\"extensions\":{\"example.test\":{\"integer\":9007199254740991,\"number\":1.25,\"null\":null,\"empty_array\":[],\"empty_object\":{}}}}"))

(test v090-roundtrip
  (let* ((schema (starintel::load-v090-schema))
         (document (v090-test-document))
         (result (starintel::roundtrip-v090-document document schema)))
    (is (equalp document result))))

(test v090-missing-required
  (let* ((schema (starintel::load-v090-schema))
         (document (v090-test-document)))
    (remhash "_id" document)
    (handler-case
        (progn
          (starintel::validate-v090-document document schema)
          (fail "missing _id was accepted"))
      (starintel::starintel-validation-error (condition)
        (is (string= "missing_required_field"
                     (starintel::validation-category condition)))))))

(test v090-unsupported-version
  (let* ((schema (starintel::load-v090-schema))
         (document (v090-test-document)))
    (setf (gethash "schema_version" document) "0.8.0")
    (handler-case
        (progn
          (starintel::validate-v090-document document schema)
          (fail "unsupported version was accepted"))
      (starintel::starintel-validation-error (condition)
        (is (string= "unsupported_spec_version"
                     (starintel::validation-category condition)))))))
