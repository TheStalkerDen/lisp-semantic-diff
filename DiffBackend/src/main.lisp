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
(defparameter *lex-errors-msgs-1* nil)
(defparameter *lex-errors-msgs-2* nil)
(defparameter *parser-error-msg-1* nil)
(defparameter *parser-error-msg-2* nil)
(defparameter *lexems-1* nil)
(defparameter *lexems-2* nil)

(defun main ()
  (let ((cmd-args (uiop:command-line-arguments)))
    (differ-v01 (first cmd-args) (second cmd-args))))

(defun differ-v01 (file1 file2)
  (init-stats)
  (let (*comment-table-1*
        *comment-table-2*
        *lex-errors-msgs-1*
        *lex-errors-msgs-2*
        *parser-error-msg-1*
        *parser-error-msg-2*
        *lexems-1*
        *lexems-2*)
    (multiple-value-bind (res1 res2)
        (compare-two-files file1 file2)
      (generate-json-outputs res1 res2))))

(defun str-differ-v01 (str1 str2)
  (init-stats)
  (let (*comment-table-1*
        *comment-table-2*
        *lex-errors-msgs-1*
        *lex-errors-msgs-2*
        *parser-error-msg-1*
        *parser-error-msg-2*
        *lexems-1*
        *lexems-2*)
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
  (when *lexems-1*
    (with-open-file (stream "lexems1.json" :direction :output
                                           :if-exists :supersede)
      (get-lexems-json *lexems-1* stream))
    (when *lex-errors-msgs-1*
      (with-open-file (stream "lexer-errors-msgs1.json" :direction :output
                                                        :if-exists :supersede)
        (get-lexer-errors-msgs-json *lex-errors-msgs-1* stream)))
    (when *parser-error-msg-1*
      (with-open-file (stream "parser-error-msg1.json" :direction :output
                                                        :if-exists :supersede)
        (get-parser-error-msg-json *parser-error-msg-1* stream))) )
  (when *lexems-2*
    (with-open-file (stream "lexems2.json" :direction :output
                                           :if-exists :supersede)
      (get-lexems-json *lexems-2* stream))
    (when *lex-errors-msgs-2*
      (with-open-file (stream "lexer-errors-msgs2.json" :direction :output
                                                        :if-exists :supersede)
        (get-lexer-errors-msgs-json *lex-errors-msgs-2* stream)))
    (when *parser-error-msg-2*
      (with-open-file (stream "parser-error-msg2.json" :direction :output
                                                        :if-exists :supersede)
        (get-parser-error-msg-json *parser-error-msg-2* stream))))
  (when (and res1 res2)
    (with-open-file (stream "res1.json" :direction :output
                                        :if-exists :supersede)
      (get-json-res res1 stream))
    (with-open-file (stream "res2.json" :direction :output
                                        :if-exists :supersede)
      (get-json-res res2 stream))
    (with-open-file (stream "stats.json" :direction :output
                                         :if-exists :supersede)
      (get-stats-res stream))))

(defun simple-differ-str (str1 str2 &optional (out1 t ) (out2 t) (out3 t))
  (init-stats)
  (let (*comment-table-1*
        *comment-table-2*
        *lex-errors-msgs-1*
        *lex-errors-msgs-2*
        *parser-error-msg-1*
        *parser-error-msg-2*
        *lexems-1*
        *lexems-2*)
    (multiple-value-bind (res1 res2)
        (compare-two-str str1 str2)
      (when *comment-table-1*
        (format t "Comment Table of str1:~%")
        (get-json-comments *comment-table-1* t))
      (when *comment-table-2*
        (get-json-comments *comment-table-2* t))
      (when *lex-errors-msgs-1*
        (format t "~%Lex-errors-msgs-1:~%")
        (get-lexer-errors-msgs-json *lex-errors-msgs-1* t)
        (format t "~%Lexems-1:~%")
        (get-lexems-json *lexems-1* t))
      (when *lex-errors-msgs-2*
        (format t "~%Lex-errors-msgs-1:~%")
        (get-lexer-errors-msgs-json *lex-errors-msgs-2* t)
        (format t "~%Lexems-1:~%")
        (get-lexems-json *lexems-2* t))
      (when *parser-error-msg-1*
        (format t "~%Parser-error-msg-1:~%")
        (get-parser-error-msg-json *parser-error-msg-1* t)
        (format t "~%Lexems-1:~%")
        (get-lexems-json *lexems-1* t))
      (when *parser-error-msg-2*
        (format t "~%Parser-error-msg-2:~%")
        (get-parser-error-msg-json *parser-error-msg-2* t)
        (format t "~%Lexems-2:~%")
        (get-lexems-json *lexems-2* t))
      (when (and res1 res2)
        (format t "~%Res1:~%")
        (get-json-res res1 out1)
        (format t "~%Res2:~%")
        (get-json-res res2 out2)
        (format t "~%Stats:~%")
        (get-stats-res out3)))))

(defun compare-two-files (filepath1 filepath2)
  (let ((str1 (read-file-into-string filepath1))
        (str2 (read-file-into-string filepath2)))
    (compare-two-str str1 str2)))

(defun compare-two-str (str1 str2)
  (let ((ast-tree-1 (get-abstract-sem-tree-from-string str1 1))
        (ast-tree-2 (get-abstract-sem-tree-from-string str2 2)))
    (when (and ast-tree-1 ast-tree-2)
      (compare-results))
    (values ast-tree-1 ast-tree-2)))

(defun get-abstract-sem-tree-from-string (str cur-file)
  (multiple-value-bind (res-lexems comments lex-errors)
      (lexer str)
    (when (> (hash-table-count comments) 0)
      (cond ((= cur-file 1)
             (setf *comment-table-1* comments))
            ((= cur-file 2)
             (setf *comment-table-2* comments))
            (t (error "Error value of cur-file"))))
    (when lex-errors
        (cond ((= cur-file 1)
               (setf *lex-errors-msgs-1* lex-errors)
               (setf *lexems-1* res-lexems))
              ((= cur-file 2)
               (setf *lex-errors-msgs-2* lex-errors)
               (setf *lexems-2* res-lexems))
              (t (error "Error value of cur-file")))
        (return-from get-abstract-sem-tree-from-string nil))
    (multiple-value-bind (res-syn-tree parser-error)
        (parser res-lexems)
      (when parser-error
        (cond ((= cur-file 1)
               (setf *parser-error-msg-1* parser-error)
               (setf *lexems-1* res-lexems))
              ((= cur-file 2)
               (setf *parser-error-msg-2* parser-error)
               (setf *lexems-2* res-lexems))
              (t (error "Error value of cur-file")))
        (return-from get-abstract-sem-tree-from-string nil))
      (abstract-sem-tree-gen res-syn-tree :curr-file cur-file))))
