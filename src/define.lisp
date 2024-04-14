(in-package :starintel)



(defun create (type &rest args)
  (apply #'make-instance type args))




(defun -> (source target &optional note)

  (let* ((source-doc (apply (car source) (cdr source)))
         (target-doc (apply (car target) (cdr target)))
         (relation

           (make-instance 'relation :source (doc-id source-doc)

                          :target (doc-id target-doc)

                          :note note)))
    (list source-doc target-doc relation)))

(defun define-docs (&rest documents)
  (loop for doc in documents
        for op = (car doc)
        collect (apply op (cdr doc))))

(defun read-star-file (pathname)
  (with-open-file (stream pathname)
    (let ((docs (read stream)))
      (flatten (apply #'define-docs docs)))))
