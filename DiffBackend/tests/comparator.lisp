(uiop:define-package :diff-backend/tests/comparator
    (:use :cl
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/comparator)

(def-simple-classifier-test only-no-mod-defuns.1
    "(defun a () 1)"
  "(defun a () 1)"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test only-no-mod-defuns.2
    "(defun a () 1)
     (defun b () 2)"
  "(defun b () 2)
   (defun a () 1)"
  `((:defuns
        (:no-mod
         ("a" "b"))))
  `((:defuns
        (:no-mod
         ("b" "a")))))

(def-simple-classifier-test completely-different.1
    "(defun b () 1)"
  "(defun a () 1)"
  `((:defuns
        (:deleted
         ("b"))))
  `((:defuns
        (:new
         ("a")))))
