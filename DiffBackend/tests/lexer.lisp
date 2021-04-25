(uiop:define-package :diff-backend/tests/lexer
    (:use :cl
          :diff-backend/lexer
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/lexer)

(declaim (optimize (debug 3)))

(def-lexer-test parent.1
    "()"
  (list
   (make-lexem "(" 1 1 :left-parent)
   (make-lexem ")" 1 2 :right-parent)))

(def-lexer-test parent.2
    "(())"
  (list
   (make-lexem "(" 1 1 :left-parent)
   (make-lexem "(" 1 2 :left-parent)
   (make-lexem ")" 1 3 :right-parent)
   (make-lexem ")" 1 4 :right-parent)))

(def-lexer-test parent.3
    "
(
 ( )
    )"
  (list
   (make-lexem "(" 2 1 :left-parent)
   (make-lexem "(" 3 2 :left-parent)
   (make-lexem ")" 3 4 :right-parent)
   (make-lexem ")" 4 5 :right-parent)))

(def-lexer-test symbol.1
    "hello"
  (list
   (make-lexem "hello" 1 1 :symbol)))

(def-lexer-test symbol.2
    "defun fun"
  (list
   (make-lexem "defun" 1 1 :symbol)
   (make-lexem "fun" 1 7 :symbol)))

(def-lexer-test symbol.3
    "1+ 1a"
  (list
   (make-lexem "1+" 1 1 :symbol)
   (make-lexem "1a" 1 4 :symbol)))

(def-lexer-test symbol.4
    "!@$%^&*-=_<>?."
  (list
   (make-lexem "!@$%^&*-=_<>?." 1 1 :symbol)))

(def-lexer-test integer
    "12345"
  (list
   (make-lexem "12345" 1 1 :integer)))

(def-lexer-test quote.1
    "'a"
  (list
   (make-lexem "'" 1 1 :quote)
   (make-lexem "a" 1 2 :symbol)))

(def-lexer-test quote.2
    "'()"
  (list
   (make-lexem "'" 1 1 :quote)
   (make-lexem "(" 1 2 :left-parent)
   (make-lexem ")" 1 3 :right-parent)))

(def-lexer-test quote.3
    "'a '1"
 (list
   (make-lexem "'" 1 1 :quote)
   (make-lexem "a" 1 2 :symbol)
   (make-lexem "'" 1 4 :quote)
   (make-lexem "1" 1 5 :integer)))

(def-lexer-test string.1
  "\"\""
  (list
   (make-lexem "\"\"" 1 1 :string)))

(def-lexer-test string.2
  "\"ab 23 ff3\""
  (list
   (make-lexem "\"ab 23 ff3\"" 1 1 :string)))

(def-lexer-test string.3
  "\"ab 23 \\\" \\\" ff3\""
  (list
   (make-lexem "\"ab 23 \\\" \\\" ff3\"" 1 1 :string)))

(def-lexer-test string.4
"\"
ab
\""
  (list
   (make-lexem
"\"
ab
\"" 1 1 :string)))

(def-lexer-test comment.1
  ";"
  nil
  :exp-comments `((1 . (:comment ";"
                        :column 1))))

(def-lexer-test comment.2
  ";;;comment"
  nil
  :exp-comments `((1 . (:comment ";;;comment"
                        :column 1))))

(def-lexer-test comment.3
  "    ;comment 1"
  nil
  :exp-comments `((1 . (:comment ";comment 1"
                        :column 5))))

(def-lexer-test comment.4
  "(fun   ;comment 1"
  (list
   (make-lexem "(" 1 1 :left-parent)
   (make-lexem "fun" 1 2 :symbol))
  :exp-comments `((1 . (:comment ";comment 1"
                        :column 8))))

(def-lexer-test comment.5
  "(fun   ;comment 1

;comment 2 ; aha"
  (list
   (make-lexem "(" 1 1 :left-parent)
   (make-lexem "fun" 1 2 :symbol))
  :exp-comments `((1 . (:comment ";comment 1"
                        :column 8))
                  (3 . (:comment ";comment 2 ; aha"
                        :column 1))))

(def-lexer-test mixed.1
    "(defun a () 1)"
  (list
   (make-lexem "(" 1 1 :left-parent)
   (make-lexem "defun" 1 2 :symbol)
   (make-lexem "a" 1 8 :symbol)
   (make-lexem "(" 1 10 :left-parent)
   (make-lexem ")" 1 11 :right-parent)
   (make-lexem "1" 1 13 :integer)
   (make-lexem ")" 1 14 :right-parent)))
