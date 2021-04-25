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
   :abstract-sem-tree-gen)
  (:import-from :diff-backend/statistics
                :init-stats))
(in-package :diff-backend)

(defparameter *comment-table-1* nil)
(defparameter *comment-table-2* nil)

(defun main ()
  (let ((cmd-args (uiop:command-line-arguments)))
    (print cmd-args)
    (differ-v01 (first cmd-args) (second cmd-args))))


(defun differ-v01 (file1 file2)
  (init-stats)
  (let (*comment-table-1*
        *comment-table-2*)
    (multiple-value-bind (res1 res2)
        (compare-two-files file1 file2)
      (generate-json-outputs res1 res2))))

(defun str-differ-v01 (str1 str2)
  (init-stats)
  (let (*comment-table-1*
        *comment-table-2*)
    (multiple-value-bind (res1 res2)
        (compare-two-str str1 str2)
      (generate-json-outputs res1 res2))))

(defun generate-json-outputs (res1 res2)
  (when *comment-table-1*
    (with-open-file (stream "comments1.json" :direction :output
                                             :if-exists :supersede)
      (get-json-comments *comment-table-1* stream)))
  (when *comment-table-2*
    (with-open-file (stream "comments2.json" :direction :output
                                             :if-exists :supersede)
      (get-json-comments *comment-table-2* stream)))
  (with-open-file (stream "res1.json" :direction :output
                                      :if-exists :supersede)
    (get-json-res res1 stream))
  (with-open-file (stream "res2.json" :direction :output
                                      :if-exists :supersede)
    (get-json-res res2 stream))
  (with-open-file (stream "stats.json" :direction :output
                                       :if-exists :supersede)
    (get-stats-res stream)))

(defun simple-differ-str (str1 str2 &optional (out1 t ) (out2 t) (out3 t))
  (init-stats)
  (let (*comment-table-1*
        *comment-table-2*)
    (multiple-value-bind (res1 res2)
        (compare-two-str str1 str2)
      (when *comment-table-1*
        (get-json-comments *comment-table-1* t))
      (when *comment-table-2*
        (get-json-comments *comment-table-2* t))
      (get-json-res res1 out1)
      (get-json-res res2 out2)
      (get-stats-res out3))))

(defun compare-two-files (filepath1 filepath2)
  (let ((str1 (read-file-into-string filepath1))
        (str2 (read-file-into-string filepath2)))
    (compare-two-str str1 str2)))

(defun compare-two-str (str1 str2)
  (let ((ast-tree-1 (get-abstract-sem-tree-from-string str1 1))
        (ast-tree-2 (get-abstract-sem-tree-from-string str2 2)))
    (compare-results)
    (values ast-tree-1 ast-tree-2)))

(defun get-abstract-sem-tree-from-string (str cur-file)
  (multiple-value-bind (res-lexems comments lex-errors)
      (lexer str)
    (when lex-errors
      (error "Lex errors !!! ~A" lex-errors))
    (when (> (hash-table-count comments) 0)
      (cond ((= cur-file 1)
             (setf *comment-table-1* comments))
            ((= cur-file 2)
             (setf *comment-table-2* comments))
            (t (error "Error value of cur-file"))))
    (abstract-sem-tree-gen (parser res-lexems) :curr-file cur-file)))
