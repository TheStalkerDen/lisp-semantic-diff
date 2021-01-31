(uiop:define-package :diff-backend
  (:use :cl :diff-backend/comparator
        :diff-backend/results-generator)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :diff-backend/lexer
                :lexer)
  (:import-from :diff-backend/parser
                :parser)
  (:import-from :diff-backend/abstract-sem-tree-generator
                :abstract-sem-tree-gen))
(in-package :diff-backend)


(defun main ()
  (let ((cmd-args (uiop:command-line-arguments)))
    (print cmd-args)
    (true-differ-v01 (first cmd-args) (second cmd-args))))


(defun true-differ-v01 (file1 file2)
 (multiple-value-bind (res1 res2)
     (compare-two-files file1 file2)
   (with-open-file (stream1 "res1.json" :direction :output
                            :if-exists :supersede)
     (with-open-file (stream2 "res2.json" :direction :output
                              :if-exists :supersede)
       (get-json-res res1 stream1)
       (get-json-res res2 stream2)))))

(defun differ-v01 (str1 str2)
 (multiple-value-bind (res1 res2)
     (compare-two-str str1 str2)
   (with-open-file (stream1 "res1.json" :direction :output
                            :if-exists :supersede)
     (with-open-file (stream2 "res2.json" :direction :output
                              :if-exists :supersede)
       (get-json-res res1 stream1)
       (get-json-res res2 stream2)))))

(defun simple-differ-str (str1 str2 &optional (out1 t ) (out2 t))
  (multiple-value-bind (res1 res2)
      (compare-two-str str1 str2)
    (get-json-res res1 out1)
    (get-json-res res2 out2)))

(defun compare-two-files (filepath1 filepath2)
  (let ((str1 (read-file-into-string filepath1))
        (str2 (read-file-into-string filepath2)))
    (compare-two-str str1 str2)))

(defun compare-two-str (str1 str2)
  (let ((ast-tree-1 (get-abstract-sem-tree-from-string str1))
        (ast-tree-2 (get-abstract-sem-tree-from-string str2)))
    (start-compare ast-tree-1 ast-tree-2)
    (values ast-tree-1 ast-tree-2)))

(defun get-abstract-sem-tree-from-string (str)
  (abstract-sem-tree-gen (parser (lexer str))))
