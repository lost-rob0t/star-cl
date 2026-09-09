(in-package :starintel-test)

(in-suite starintel-test)

(test operation-is-control-plane-document
  (let* ((data (jsown:empty-object))
         (phases (list (let ((phase (jsown:empty-object)))
                         (setf (jsown:val phase "phase_id") "discovery"
                               (jsown:val phase "objective") "Discover public surfaces"
                               (jsown:val phase "state") "ready")
                         phase))))
    (setf (jsown:val data "mission") "Coordinate phased research"
          (jsown:val data "status") "active"
          (jsown:val data "phases") phases)
    (let* ((op (make-instance 'operation
                              :dataset "test-operation"
                              :data data))
           (encoded (encode op)))
      (is (string= "operation" (doc-type op)))
      (is (string= "Action" (jsown:val (doc-schema-org op) "@type")))
      (is (member "operation" (registered-document-dtypes) :test #'string=))
      (is (string= "Coordinate phased research"
                   (jsown:val (jsown:val encoded "data") "mission")))
      (is (typep (decode-document encoded) 'operation)))))
