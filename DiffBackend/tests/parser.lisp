(uiop:define-package :diff-backend/parser
    (:use :cl
          :diff-backend/parser
          :diff-backend/lexer
          :rove))

(in-package :diff-backend/parser)

(defmacro def-parser-test (name str parser-exp)
  `(deftest ,name
       (let ((parser-res (parser (lexer ,str))))
         (unless
             (tree-equal
              parser-res
              ,parser-exp
              :test (lambda (x y)
                      (typecase x
                        (lexem (when (typep y 'lexem)
                                 (equal-lexem? x y)))
                        (t (eq x y)))))
           (fail "Parser-test failed")))))

(def-parser-test atom.1
    "1"
  `(:top
    nil
    (:s-expr
     nil
     (:atom
      nil
      ,(make-lexem "1" 1 1 :integer)))))

(def-parser-test atom.2
    "a 1"
  `(:top
    nil
    (:s-expr
     nil
     (:atom
      nil
      ,(make-lexem "a" 1 1 :symbol)))
    (:s-expr
     nil
     (:atom
      nil
      ,(make-lexem "1" 1 3 :integer)))))

(def-parser-test list.1
    "()"
  `(:top
    nil
    (:s-expr
     nil
     (:list
      ((:lparen-coord 1 1)
       (:rparen-coord 1 2))))))

(def-parser-test list.2
    "(())"
  `(:top
    nil
    (:s-expr
     nil
     (:list
      ((:lparen-coord 1 1)
       (:rparen-coord 1 4))
      (:s-expr
       nil
       (:list
        ((:lparen-coord 1 2)
         (:rparen-coord 1 3))))))))
