(require :asdf)
(let ((*standard-output* *error-output*)) (asdf:load-system :starintel))
(let* ((fixture (merge-pathnames "fixtures/raw-json-unique-keys.json" *load-truename*))
       (cases (gethash "cases" (starintel:parse-json fixture)))
       (count 0))
  (loop for item across cases
        for name = (gethash "name" item)
        for raw = (gethash "wire" item)
        for valid = (gethash "valid" item)
        do (dolist (parser (list #'starintel:parse-json #'starintel.canonical:parse-json
                                #'starintel:from-json #'starintel:decode-document))
             (let ((accepted t) (message ""))
               (handler-case (funcall parser raw)
                 (com.inuoe.jzon:json-parse-error (condition)
                   (setf accepted nil message (princ-to-string condition))))
               (assert (eq accepted valid) () "~a: accepted=~s expected=~s" name accepted valid)
               (unless valid
                 (assert (search "Duplicate JSON key" message) () "Unexpected rejection: ~a" message))))
           (when valid
             (let* ((parsed (starintel:parse-json raw))
                    (encoded (starintel:stringify-json parsed)))
               (assert (equal encoded (starintel:stringify-json (starintel:parse-json encoded))))))
           (incf count))
  (format t "~d shared raw JSON key cases passed through four public boundaries.~%" count))
