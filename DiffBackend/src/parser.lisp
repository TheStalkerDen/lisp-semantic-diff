(uiop:define-package :diff-backend/parser
    (:nicknames :parser)
  (:use :cl :diff-backend/lexer
        :diff-backend/utils)
  (:export #:parser))

(in-package :diff-backend/parser)

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
  (setf *cur-lex* (first *lexems-rest*))
  (setf *lexems-rest* (rest *lexems-rest*))
  *cur-lex*)

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
  (let ((*lexems-rest* lexems))
    (catch 'parser-start
      (top-rule))))

(defun top-rule ()
  (let (s-expr-l)
    (loop :while *lexems-rest*
          :do (push (s-expr-rule) s-expr-l))
    `(:top () ,@(reverse s-expr-l))))

(defun s-expr-rule (&optional lex)
  (ecase (lexem-type (or lex (next-lexem)))
    ((:integer :symbol :string)
     (atom-rule *cur-lex*))
    ((:left-parent)
     (list-rule *cur-lex*))
    ((:quote)
     (quote-s-expr-rule *cur-lex*))
    ((:right-parent)
     (throw-error "unmatched close parenthesis" *cur-lex*))))

(defun atom-rule (lex)
  `(:atom () ,lex))

(defun quote-s-expr-rule (lex)
  (let ((next-lex (next-lexem)))
    (when (or (null next-lex)
              (eq (lexem-type next-lex) :right-parent))
      (throw-error "no s-expr after '" lex))
    `(:quote
      ((:coord ,(lexem-line lex)
               ,(lexem-column lex)))
      ,(s-expr-rule next-lex))))

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
