(in-package :starintel)

(defun create (type &rest args)
  (apply #'make-instance type args))

(defun -> (source target &optional note)
  (let* ((source-doc (apply (car source) (cdr source)))
         (target-doc (apply (car target) (cdr target)))
         (relation
           (make-instance 'relation
                          :source (doc-id source-doc)
                          :target (doc-id target-doc)
                          :note (or note ""))))
    (list source-doc target-doc relation)))

(defun define-docs (&rest documents)
  (flatten
   (loop for document in documents
         for operation = (car document)
         collect (apply operation (cdr document)))))

(defun read-star-file (pathname)
  (with-open-file (stream pathname)
    (let ((documents (read stream)))
      (apply #'define-docs documents))))
