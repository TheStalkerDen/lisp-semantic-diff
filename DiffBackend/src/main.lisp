(defpackage diff-backend
  (:use :cl :diff-backend/comparator)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :diff-backend/lexer
                :lexer)
  (:import-from :diff-backend/parser
                :parser)
  (:import-from :diff-backend/abstract-sem-tree-generator
                :abstract-sem-tree-gen))
(in-package :diff-backend)

;; blah blah blah.
(defun main ())

(defun compare-two-files (file1 file2)
  (let ((str1 (read-file-into-string file1))
        (str2 (read-file-into-string file2)))
     (compare-two-str str1 str2)))

(defun compare-two-str (str1 str2)
  (let ((ast-tree-1 (get-abstract-sem-tree-from-string str1))
        (ast-tree-2 (get-abstract-sem-tree-from-string str2)))
    (compare ast-tree-1 ast-tree-2)
    (values ast-tree-1 ast-tree-2)))

(defun get-abstract-sem-tree-from-string (str)
  (abstract-sem-tree-gen (parser (lexer str))))
