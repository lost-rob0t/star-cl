(defpackage :starintel-test
  (:use :cl :fiveam :starintel)
  (:export #:run-tests
           #:starintel-test))

(in-package :starintel-test)

(def-suite starintel-test
  :description "Main test suite for Star Intel")

(defun run-tests ()
  "Run all tests in the Star Intel test suite"
  (run! 'starintel-test))
