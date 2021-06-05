(uiop:define-package :diff-backend/tests/statistics
    (:use :cl
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/statistics)

(def-stats-test defuns.1
  "(defun a () 1)"
  `((:defuns
        ("a"))))

(def-stats-test defuns.2
  "(defun a () 1)
(defun b () 2)"
  `((:defuns
        ("a" "b"))))

(def-stats-test defuns.3
  "(defun a () 1)
(defun b () 2)
(defun c () 3)"
  `((:defuns
        ("a" "b" "c"))))

(def-stats-test defparameter.1
  "(defparameter a 1)"
  `((:defparameters
        ("a"))))

(def-stats-test mixed.1
  "(defparameter a 1)
(defun b () 2) "
  `((:defparameters
        ("a"))
    (:defuns
        ("b"))))

