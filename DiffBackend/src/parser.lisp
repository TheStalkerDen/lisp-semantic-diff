(uiop:define-package :diff-backend/parser
    (:nicknames :parser)
  (:use :cl :diff-backend/lexer
        :diff-backend/utils)
  (:import-from :alexandria
   :eswitch)
  (:export #:parser))

(in-package :diff-backend/parser)

(declaim (optimize safety))

(defvar *lexems-rest* nil
  "Stores rest part of lexems")

(defvar *cur-lex* nil)

(defclass* parser-error-info ()
    ((error-text
      :accessor error-text
      :initarg :error-text)
     (error-lex-id
      :accessor error-lex-id
      :initarg :error-lex-id)))

(defun next-lexem ()
  (progn
    (setf *cur-lex* (first *lexems-rest*))
    (setf *lexems-rest* (rest *lexems-rest*))
    *cur-lex*))

(defun throw-error (error-text error-lex)
  (throw 'parser-start
       (values nil
               (make-instance
                'parser-error-info
                :error-text
                (format nil "At (~S:~S) ~A"
                        (lexem-line error-lex)
                        (lexem-column error-lex)
                        error-text)
                :error-lex-id (id error-lex)))))

(defun parser (lexems)
  (let ((*lexems-rest* lexems)
        (s-expr-l))
    (catch 'parser-start
      (loop :while *lexems-rest*
            :do (push (s-expr-rule (next-lexem)) s-expr-l))
      `(:top () ,@(reverse s-expr-l)))))

(defun s-expr-rule (lex)
  (ecase (lexem-type lex)
    ((:integer :symbol :string)
     `(:atom () ,lex))
    ((:left-parent)
     (list-rule lex))
    ((:quote)
     (let ((next-lex (next-lexem)))
       (when (or (null next-lex)
                 (eq (lexem-type next-lex) :right-parent))
         (throw-error "no s-expr after '" lex))
       `(:quote
         ((:coord ,(lexem-line lex)
                  ,(lexem-column lex)))
         ,(s-expr-rule next-lex))))
    ((:right-parent)
     (throw-error "unmatched close parenthesis" lex))))

(defun list-rule (left-parent-lexem)
  (do ((s-expr-l nil)
       (lex (next-lexem) (next-lexem)))
      ((when lex (eq (lexem-type lex) :right-parent))
       `(:list ((:lparen-coord ,(lexem-line left-parent-lexem)
                               ,(lexem-column left-parent-lexem))
                (:rparen-coord ,(lexem-line lex)
                               ,(lexem-column lex)))
               ,@(reverse s-expr-l)))
    (unless lex
      (throw-error "unclosed parenthesis" left-parent-lexem))
    (push (s-expr-rule lex) s-expr-l)))
