(uiop:define-package :diff-backend/tests/abstract-sem-tree-generator
    (:use :cl
          :diff-backend/nodes
          :diff-backend/lexer
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/abstract-sem-tree-generator)

(def-ast-test atom.1
    "1"
  (list
   (make-instance
    'lexem-wrapper-node
    :lexem-info (make-lexem
                 "1"
                 1
                 1
                 :integer))))

(def-ast-test funcall.1
    "(fun)"
  (list
   (make-instance
    'function-call-node
    :func-lexem (make-instance
                 'lexem-wrapper-node
                 :lexem-info (make-lexem
                              "fun"
                              1
                              2
                              :symbol))
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 5))
    :func-arg-forms ())))

(def-ast-test funcall.2
    "(fun 1)"
  (list
   (make-instance
    'function-call-node
    :func-lexem (make-instance
                 'lexem-wrapper-node
                 :lexem-info (make-lexem
                              "fun"
                              1
                              2
                              :symbol))
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 7))
    :func-arg-forms `(,(make-instance
                        'lexem-wrapper-node
                        :lexem-info
                        (make-lexem
                         "1"
                         1
                         6
                         :integer))))))

(def-ast-test defun.1
    "(defun a () 1)"
  (list
   (make-instance
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

(def-ast-test defun.2
    "(defun a (b) b)"
  (list
   (make-instance
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
                        (:rparen-coord 1 15))
    :parameters-list (make-instance
                      'list-node
                      :parenthesis-info `((:lparen-coord 1 10)
                                          (:rparen-coord 1 12))
                      :elements `(,(make-instance
                                    'lexem-wrapper-node
                                    :lexem-info (make-lexem
                                                 "b"
                                                 1
                                                 11
                                                 :symbol))))
    :body-forms `(,(make-instance
                    'lexem-wrapper-node
                    :lexem-info (make-lexem
                                 "b"
                                 1
                                 14
                                 :symbol))))))
