(uiop:define-package :diff-backend/tests/parser
    (:use :cl
     :diff-backend/lexer
          :diff-backend/parser
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/parser)

(def-parser-test atom.1
    "1"
  `(:top
    nil
    (:atom
     nil
     ,(make-lexem "1" 1 1 :integer))))

(def-parser-test atom.2
    "a 1"
  `(:top
    nil
    (:atom
     nil
     ,(make-lexem "a" 1 1 :symbol))
    (:atom
     nil
     ,(make-lexem "1" 1 3 :integer))))

(def-parser-test list.1
    "()"
  `(:top
    nil
    (:list
     ((:lparen-coord 1 1)
      (:rparen-coord 1 2)))))

(def-parser-test list.2
    "(())"
  `(:top
    nil
    (:list
     ((:lparen-coord 1 1)
      (:rparen-coord 1 4))
     (:list
      ((:lparen-coord 1 2)
       (:rparen-coord 1 3))))))

(def-parser-test quote.1
    "'a"
  `(:top
    nil
    (:quote
     ((:coord 1 1))
     (:atom
      nil
      ,(make-lexem "a" 1 2 :symbol)))))

(def-parser-test quote.2
    "'()"
  `(:top
    nil
    (:quote
     ((:coord 1 1))
     (:list
      ((:lparen-coord 1 2)
       (:rparen-coord 1 3))))))

(def-parser-test mixed.1
    "(a)"
  `(:top
    nil
    (:list
     ((:lparen-coord 1 1)
      (:rparen-coord 1 3))
     (:atom
      nil
      ,(make-lexem "a" 1 2 :symbol)))))


(def-parser-test error.1
  "("
  nil
  :exp-parser-error
  (make-instance
   'parser-error-info
   :error-text "At (1:1) unclosed parenthesis"
   :error-lex-id 1 ))

(def-parser-test error.2
  ")"
  nil
  :exp-parser-error
  (make-instance
   'parser-error-info
   :error-text "At (1:1) unmatched close parenthesis"
   :error-lex-id 1))

(def-parser-test error.3
  "'"
  nil
  :exp-parser-error
  (make-instance
   'parser-error-info
   :error-text "At (1:1) no s-expr after '"
   :error-lex-id 1))

(def-parser-test error.4
  "(')"
  nil
  :exp-parser-error
  (make-instance
   'parser-error-info
   :error-text "At (1:2) no s-expr after '"
   :error-lex-id 2))
