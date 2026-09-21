(defpackage :starintel-archive-test
  (:use :cl :fiveam :starintel.archive)
  (:export #:run-tests))

(in-package :starintel-archive-test)

(def-suite archive-test
  :description "StarIntel archive core")

(defun run-tests ()
  (run! 'archive-test))