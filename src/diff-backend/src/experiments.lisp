(uiop:define-package :diff-backend/experiments
    (:use :cl :diff-backend/comparator
          :diff-backend/results-generator)
  (:import-from :alexandria
   :read-file-into-string)
  (:import-from :diff-backend/lexer
   :lexer)
  (:import-from :diff-backend/parser
   :parser)
  (:import-from :diff-backend/abstract-sem-tree-generator
   :abstract-sem-tree-gen)
  (:import-from :diff-backend/statistics
                :init-stats))
(in-package :diff-backend/experiments)

(defun expers-with-equal-files ()
  (dotimes (i 17)
    (compare-two-files (format nil "../../out/f~Sv1.lisp" (+ i 1))
                       (format nil "../../out/f~Sv2.lisp" (+ i 1)))))

(defun compare-two-files (filepath1 filepath2)
  (let ((str1 (read-file-into-string filepath1))
        (str2 (read-file-into-string filepath2)))
    (init-stats)
    (compare-two-str str1 str2)))

(defun compare-two-str (str1 str2)
  (let ((ast-1 (get-abstract-sem-tree-from-string str1 1))
        (ast-2 (get-abstract-sem-tree-from-string str2 2))
        (start-time (get-internal-run-time)))
    (multiple-value-bind (m-ast-1 m-ast-2 moved-s-exprs-list)
        (start-asts-comparing ast-1 ast-2)
      (log:info "Exec time: ~S" (- (get-internal-run-time) start-time))
      (return-from compare-two-str (values m-ast-1 m-ast-2)))))

(defun get-abstract-sem-tree-from-string (str cur-file)
  (multiple-value-bind (res-lexems comments lex-errors)
      (lexer str)
    (multiple-value-bind (res-syn-tree parser-error)
        (parser res-lexems)
      (multiple-value-bind (res-ast semantic-errors)
          (abstract-sem-tree-gen res-syn-tree :curr-file cur-file)
        res-ast))))
