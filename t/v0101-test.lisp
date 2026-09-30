(in-package :starintel-test)

(in-suite starintel-test)

(defun object-value (object key)
  (gethash key object))

(test generated-star-lang-binding-is-loaded
  (let ((package (find-package :org.starintel.core.v1)))
    (is (not (null package)))
    (is (eq :external
            (nth-value 1 (find-symbol "PERSON-IDENTIFIER" package))))
    (is (eq :external
            (nth-value 1 (find-symbol "GEO-POINT-LATITUDE" package))))))

(test v0101-shared-migration-fixtures
  (let* ((fixtures (starintel-v0101:load-fixtures))
         (compatibility (starintel-v0101:load-compatibility))
         (schema (starintel-v0101:load-schema)))
    (loop for fixture across (object-value fixtures "cases")
          for result =
            (starintel-v0101:migrate-batch
             (vector (object-value fixture "input"))
             compatibility schema)
          do (is (equalp (object-value fixture "expected") result)
                 "Fixture failed: ~a" (object-value fixture "name")))))

(test v0101-migration-is-idempotent
  (let* ((fixtures (starintel-v0101:load-fixtures))
         (input (object-value (aref (object-value fixtures "cases") 0) "input"))
         (first (starintel-v0101:migrate-batch (vector input)))
         (second
           (starintel-v0101:migrate-batch (object-value first "documents"))))
    (is (equalp first second))))

(test v0101-canonical-validation-rejects-snake-case
  (let ((document
          (starintel-v0101:json-object
           "id" "person-invalid"
           "dataset" "fixture"
           "dtype" "person"
           "schemaVersion" "0.10.1"
           "first_name" "Ada")))
    (signals starintel-v0101:starintel-validation-error
      (starintel-v0101:validate-document document))))

(test v0101-batch-quarantines-and-continues
  (let* ((fixtures (starintel-v0101:load-fixtures))
         (cases (object-value fixtures "cases"))
         (result
           (starintel-v0101:migrate-batch
            (vector (object-value (aref cases (1- (length cases))) "input")
                    (object-value (aref cases 0) "input")))))
    (is (= 1 (length (object-value result "quarantine"))))
    (is (string= "ambiguousFieldCollision"
                 (object-value (aref (object-value result "quarantine") 0)
                               "reasonCode")))
    (is (string= "person-current"
                 (object-value (aref (object-value result "documents") 0) "id")))))

(test v0101-geo-ranges-are-exact
  (let ((document
          (starintel-v0101:json-object
           "id" "geo-invalid"
           "dataset" "fixture"
           "dtype" "geo-point"
           "schemaVersion" "0.10.1"
           "geometryType" "point"
           "longitude" "180.0001"
           "latitude" "0")))
    (signals starintel-v0101:starintel-validation-error
      (starintel-v0101:validate-document document))))
