(defpackage DiffBackend/tests/main
  (:use :cl
        :DiffBackend
        :rove))
(in-package :DiffBackend/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :DiffBackend)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
