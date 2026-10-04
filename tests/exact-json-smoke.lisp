(require :asdf)
(let ((*standard-output* *error-output*)) (asdf:load-system :starintel))
(let* ((request (starintel:parse-json *standard-input*))
       (mode (gethash "mode" request))
       (input (gethash "input" request)))
  (handler-case
      (let* ((value (starintel.canonical:parse-json input))
             (output
               (cond
                 ((equal mode "root") (starintel:to-json (starintel:from-json input)))
                 ((equal mode "root-decode") (starintel:to-json (starintel:decode-document input)))
                 ((equal mode "root-create")
                  (starintel:to-json
                   (starintel:person :id "person:exact" :dataset "conformance"
                                     :extensions (gethash "extensions" value))))
                 ((equal mode "bounds")
                  (starintel::validate-v090-value value (gethash "schema" request))
                  (starintel:stringify-json value))
                 (t (starintel.canonical:stringify-json value)))))
        (write-line output))
    (error () (write-line "{\"rejected\":true}") (uiop:quit 1))))
