(uiop:define-package :diff-backend/tests/statistics
    (:use :cl
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/statistics)

(def-stats-test defuns.1
  "(defun a () 1)"
  `((:defuns
     ("a"))))
