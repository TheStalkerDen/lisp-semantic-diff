(uiop:define-package :diff-backend/tests/abstract-sem-tree-generator
    (:use :cl
          :diff-backend/nodes
          :diff-backend/lexer
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/abstract-sem-tree-generator)

(def-ast-test defun.1
    "(defun a () 1)"
  (list (make-instance
         'defun-node
         :func-name (make-instance
                     'lexem-wrapper-node
                     :lexem-info (make-lexem
                                  "a"
                                  1
                                  8
                                  :symbol))
         :keyword-lexem (make-instance
                         'lexem-wrapper-node
                         :lexem-info (make-lexem
                                      "defun"
                                      1
                                      2
                                      :symbol))
         :parenthesis-info `((:lparen-coord 1 1)
                             (:rparen-coord 1 14))
         :parameters-list (make-instance
                           'list-node
                           :parenthesis-info `((:lparen-coord 1 10)
                                               (:rparen-coord 1 11))
                           :elements ())
         :body-forms `(,(make-instance
                         'lexem-wrapper-node
                         :lexem-info (make-lexem
                                      "1"
                                      1
                                      13
                                      :integer))))))
