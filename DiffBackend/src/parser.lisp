(uiop:define-package :diff-backend/parser
    (:nicknames :parser)
  (:use :cl :diff-backend/lexer)
  (:import-from :alexandria
                :eswitch)
  (:export #:parser))

(in-package :diff-backend/parser)

(declaim (optimize safety))

(defvar *lexems-rest* nil
  "Stores rest part of lexems")

(defvar *cur-lex* nil)

(defun next-lexem ()
  (progn
    (setf *cur-lex* (first *lexems-rest*))
    (setf *lexems-rest* (rest *lexems-rest*))
    *cur-lex*))

(defun cur-lexem ()
  *cur-lex*)

(defun parser (lexems)
  (let ((*lexems-rest* lexems)
        (s-expr-l))
    (loop :while *lexems-rest*
       :do (push (s-expr-rule (next-lexem)) s-expr-l))
    `(:top () ,@s-expr-l)))

(defun s-expr-rule (lex)
  (ecase (lexem-type lex)
    ((:integer :symbol)
     `(:s-expr () (:atom () ,lex)))
    ((:left-parent)
     `(:s-expr () ,(list-rule lex)))))

(defun list-rule (left-parent-lexem)
  
  (do  ((s-expr-l nil)
        (lex (next-lexem) (next-lexem)))
       ((eq (lexem-type lex) :right-parent)
        `(:list ((:lparen-coord (,(lexem-line left-parent-lexem)
                                  ,(lexem-column left-parent-lexem)))
                 (:rparen-coord (,(lexem-line lex)
                                  ,(lexem-column lex))))
                ,@(reverse s-expr-l)))
    (push (s-expr-rule lex) s-expr-l)))
