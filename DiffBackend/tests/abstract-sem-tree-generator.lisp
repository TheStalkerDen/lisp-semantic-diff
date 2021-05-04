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
                 :integer
                 :lex-id 1)
    :id 0)))

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
                              :symbol
                              :lex-id 2)
                 :id 1)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 5))
    :func-arg-forms nil
    :id 0)))

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
                              :symbol
                              :lex-id 2)
                 :id 1)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 7))
    :func-arg-forms (vector (make-instance
                             'lexem-wrapper-node
                             :lexem-info
                             (make-lexem
                              "1"
                              1
                              6
                              :integer
                              :lex-id 3)
                             :id 2))
    :id 0)))

(def-ast-test defun.1
  "(defun a () 1)"
  (list
   (make-instance
    'defun-node
    :keyword-lexem (make-instance
                    'lexem-wrapper-node
                    :lexem-info (make-lexem
                                 "defun"
                                 1
                                 2
                                 :symbol
                                 :lex-id 2)
                    :id 1)
    :func-name (make-instance
                'lexem-wrapper-node
                :lexem-info (make-lexem
                             "a"
                             1
                             8
                             :symbol
                             :lex-id 3)
                :id 2)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 14))
    :parameters-list (make-instance
                      'list-node
                      :parenthesis-info `((:lparen-coord 1 10)
                                          (:rparen-coord 1 11))
                      :elements nil
                      :id 3)
    :body-forms (vector
                 (make-instance
                  'lexem-wrapper-node
                  :lexem-info (make-lexem
                               "1"
                               1
                               13
                               :integer
                               :lex-id 6)
                  :id 4))
    :id 0)))

(def-ast-test defun.2
  "(defun a (b) b)"
  (list
   (make-instance
    'defun-node
    :keyword-lexem (make-instance
                    'lexem-wrapper-node
                    :lexem-info (make-lexem
                                 "defun"
                                 1
                                 2
                                 :symbol
                                 :lex-id 2)
                    :id 1)
    :func-name (make-instance
                'lexem-wrapper-node
                :lexem-info (make-lexem
                             "a"
                             1
                             8
                             :symbol
                             :lex-id 3)
                :id 2)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 15))
    :parameters-list (make-instance
                      'list-node
                      :parenthesis-info `((:lparen-coord 1 10)
                                          (:rparen-coord 1 12))
                      :elements (vector (make-instance
                                         'lexem-wrapper-node
                                         :lexem-info (make-lexem
                                                      "b"
                                                      1
                                                      11
                                                      :symbol
                                                      :lex-id 5)
                                         :id 4))
                      :id 3)
    :body-forms (vector (make-instance
                         'lexem-wrapper-node
                         :lexem-info (make-lexem
                                      "b"
                                      1
                                      14
                                      :symbol
                                      :lex-id 7)
                         :id 5))
    :id 0)))

(def-ast-test defun.3
  "(defun a (b) (f b 1))"
  (list
   (make-instance
    'defun-node
    :id 0
    :keyword-lexem (make-instance
                    'lexem-wrapper-node
                    :lexem-info (make-lexem
                                 "defun"
                                 1
                                 2
                                 :symbol
                                 :lex-id 2)
                    :id 1)
    :func-name (make-instance
                'lexem-wrapper-node
                :lexem-info (make-lexem
                             "a"
                             1
                             8
                             :symbol
                             :lex-id 3)
                :id 2)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 21))
    :parameters-list (make-instance
                      'list-node
                      :parenthesis-info `((:lparen-coord 1 10)
                                          (:rparen-coord 1 12))
                      :elements (vector
                                 (make-instance
                                  'lexem-wrapper-node
                                  :lexem-info (make-lexem
                                               "b"
                                               1
                                               11
                                               :symbol
                                               :lex-id 5)
                                  :id 4))
                      :id 3)
    :body-forms (vector
                 (make-instance
                  'function-call-node
                  :id 5
                  :func-lexem (make-instance
                               'lexem-wrapper-node
                               :id 6
                               :lexem-info (make-lexem
                                            "f"
                                            1
                                            15
                                            :symbol
                                            :lex-id 8))
                  :parenthesis-info `((:lparen-coord 1 14)
                                      (:rparen-coord 1 20))
                  :func-arg-forms (vector
                                   (make-instance
                                    'lexem-wrapper-node
                                    :lexem-info (make-lexem
                                                 "b"
                                                 1
                                                 17
                                                 :symbol
                                                 :lex-id 9)
                                    :id 7)
                                   (make-instance
                                    'lexem-wrapper-node
                                    :lexem-info (make-lexem
                                                 "1"
                                                 1
                                                 19
                                                 :integer
                                                 :lex-id 10)
                                    :id 8)))))))
