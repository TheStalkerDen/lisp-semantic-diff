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
    'simple-atom-node
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
                 'func-name-atom-node
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
                 'func-name-atom-node
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
                             'simple-atom-node
                             :lexem-info
                             (make-lexem
                              "1"
                              1
                              6
                              :integer
                              :lex-id 3)
                             :id 2))
    :id 0)))

(def-ast-test quote.1
  "'a"
  (list
   (make-instance
    'quote-node
    :id 0
    :quote-coord `(1 1)
    :q-s-expr (make-instance
               'q-atom-node
               :id 1
               :lexem-info (make-lexem
                            "a"
                            1
                            2
                            :symbol
                            :lex-id 2)))))

(def-ast-test quote.2
  "'(1)"
  (list
   (make-instance
    'quote-node
    :id 0
    :quote-coord `(1 1)
    :q-s-expr (make-instance
               'q-list-node
               :id 1
               :parenthesis-info `((:lparen-coord 1 2)
                                   (:rparen-coord 1 4))
               :elements (vector
                          (make-instance
                           'q-atom-node
                           :id 2
                           :lexem-info (make-lexem
                                        "1"
                                        1
                                        3
                                        :integer
                                        :lex-id 3)))))))

(def-ast-test let.1
  "(let ()
    1)"
  (list
   (make-instance
    'let-node
    :id 0
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 2 6))
    :keyword-atom (make-instance
                   'keyword-atom-node
                   :id 1
                   :lexem-info (make-lexem
                                "let"
                                1
                                2
                                :symbol
                                :lex-id 2))
    :bindings (make-instance
               'bindings-list-node
               :id 2
               :parenthesis-info `((:lparen-coord 1 6)
                                   (:rparen-coord 1 7))
               :elements nil)
    :body-forms
    (vector
     (make-instance 'simple-atom-node
                    :id 3
                    :lexem-info (make-lexem
                                 "1"
                                 2
                                 5
                                 :integer
                                 :lex-id 5))))))

(def-ast-test let.2
  "(let (a b)
    1)"
  (list
   (make-instance
    'let-node
    :id 0
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 2 6))
    :keyword-atom (make-instance
                   'keyword-atom-node
                   :id 1
                   :lexem-info (make-lexem
                                "let"
                                1
                                2
                                :symbol
                                :lex-id 2))
    :bindings (make-instance
               'bindings-list-node
               :id 2
               :parenthesis-info `((:lparen-coord 1 6)
                                   (:rparen-coord 1 10))
               :elements (vector
                          (make-instance 'decl-var-atom-node
                                         :id 3
                                         :lexem-info (make-lexem
                                                      "a"
                                                      1
                                                      7
                                                      :symbol
                                                      :lex-id 4))
                          (make-instance 'decl-var-atom-node
                                         :id 4
                                         :lexem-info (make-lexem
                                                      "b"
                                                      1
                                                      9
                                                      :symbol
                                                      :lex-id 5))))
    :body-forms
    (vector
     (make-instance 'simple-atom-node
                    :id 5
                    :lexem-info (make-lexem
                                 "1"
                                 2
                                 5
                                 :integer
                                 :lex-id 7))))))

(def-ast-test let.3
  "(let (c (a) (b 3))
    1)"
  (list
   (make-instance
    'let-node
    :id 0
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 2 6))
    :keyword-atom (make-instance
                   'keyword-atom-node
                   :id 1
                   :lexem-info (make-lexem
                                "let"
                                1
                                2
                                :symbol
                                :lex-id 2))
    :bindings (make-instance
               'bindings-list-node
               :id 2
               :parenthesis-info `((:lparen-coord 1 6)
                                   (:rparen-coord 1 18))
               :elements (vector
                          (make-instance 'decl-var-atom-node
                                         :id 3
                                         :lexem-info (make-lexem
                                                      "c"
                                                      1
                                                      7
                                                      :symbol
                                                      :lex-id 4))
                          (make-instance 'let-binding-node
                                         :id 4
                                         :parenthesis-info `((:lparen-coord 1 9)
                                                             (:rparen-coord 1 11))
                                         :var-atom (make-instance 'decl-var-atom-node
                                                                  :id 5
                                                                  :lexem-info (make-lexem
                                                                               "a"
                                                                               1
                                                                               10
                                                                               :symbol
                                                                               :lex-id 6))
                                         :value-s-expr nil)
                          (make-instance 'let-binding-node
                                         :id 6
                                         :parenthesis-info `((:lparen-coord 1 13)
                                                             (:rparen-coord 1 17))
                                         :var-atom (make-instance 'decl-var-atom-node
                                                                  :id 7
                                                                  :lexem-info (make-lexem
                                                                               "b"
                                                                               1
                                                                               14
                                                                               :symbol
                                                                               :lex-id 9))
                                         :value-s-expr
                                         (make-instance 'simple-atom-node
                                                        :id 8
                                                        :lexem-info
                                                        (make-lexem
                                                         "3"
                                                         1
                                                         16
                                                         :integer
                                                         :lex-id 10)))))
    :body-forms
    (vector
     (make-instance 'simple-atom-node
                    :id 9
                    :lexem-info (make-lexem
                                 "1"
                                 2
                                 5
                                 :integer
                                 :lex-id 13))))))


(def-ast-test defun.1
  "(defun a () 1)"
  (list
   (make-instance
    'defun-node
    :keyword-atom (make-instance
                   'keyword-atom-node
                   :lexem-info (make-lexem
                                "defun"
                                1
                                2
                                :symbol
                                :lex-id 2)
                   :id 1)
    :func-name (make-instance
                'func-name-atom-node
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
                      'parm-list-node
                      :parenthesis-info `((:lparen-coord 1 10)
                                          (:rparen-coord 1 11))
                      :elements nil
                      :id 3)
    :body-forms (vector
                 (make-instance
                  'simple-atom-node
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
    :keyword-atom (make-instance
                    'keyword-atom-node
                    :lexem-info (make-lexem
                                 "defun"
                                 1
                                 2
                                 :symbol
                                 :lex-id 2)
                    :id 1)
    :func-name (make-instance
                'func-name-atom-node
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
                      'parm-list-node
                      :parenthesis-info `((:lparen-coord 1 10)
                                          (:rparen-coord 1 12))
                      :elements (vector (make-instance
                                         'simple-atom-node
                                         :lexem-info (make-lexem
                                                      "b"
                                                      1
                                                      11
                                                      :symbol
                                                      :lex-id 5)
                                         :id 4))
                      :id 3)
    :body-forms (vector (make-instance
                         'simple-atom-node
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
    :keyword-atom (make-instance
                    'keyword-atom-node
                    :lexem-info (make-lexem
                                 "defun"
                                 1
                                 2
                                 :symbol
                                 :lex-id 2)
                    :id 1)
    :func-name (make-instance
                'func-name-atom-node
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
                      'parm-list-node
                      :parenthesis-info `((:lparen-coord 1 10)
                                          (:rparen-coord 1 12))
                      :elements (vector
                                 (make-instance
                                  'simple-atom-node
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
                               'func-name-atom-node
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
                                    'simple-atom-node
                                    :lexem-info (make-lexem
                                                 "b"
                                                 1
                                                 17
                                                 :symbol
                                                 :lex-id 9)
                                    :id 7)
                                   (make-instance
                                    'simple-atom-node
                                    :lexem-info (make-lexem
                                                 "1"
                                                 1
                                                 19
                                                 :integer
                                                 :lex-id 10)
                                    :id 8)))))))

(def-ast-test defparameter.1
  "(defparameter a 1)"
  (list
   (make-instance
    'defparameter-node
    :keyword-atom (make-instance
                   'keyword-atom-node
                   :lexem-info (make-lexem
                                "defparameter"
                                1
                                2
                                :symbol
                                :lex-id 2)
                   :id 1)
    :parameter-name (make-instance
                     'func-name-atom-node
                     :lexem-info (make-lexem
                                  "a"
                                  1
                                  15
                                  :symbol
                                  :lex-id 3)
                     :id 2)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 18))
    :value-s-expr (make-instance
                   'simple-atom-node
                   :lexem-info (make-lexem
                                "1"
                                1
                                17
                                :integer
                                :lex-id 4)
                   :id 3)
    :id 0)))

(def-ast-test if.1
  "(if a 1)"
  (list
   (make-instance
    'if-node
    :keyword-atom (make-instance
                   'keyword-atom-node
                   :lexem-info (make-lexem
                                "if"
                                1
                                2
                                :symbol
                                :lex-id 2)
                   :id 1)
    :test-s-expr (make-instance
                  'simple-atom-node
                  :lexem-info (make-lexem
                               "a"
                               1
                               5
                               :symbol
                               :lex-id 3)
                  :id 2)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 8))
    :then-s-expr (make-instance
                  'simple-atom-node
                  :lexem-info (make-lexem
                               "1"
                               1
                               7
                               :integer
                               :lex-id 4)
                  :id 3)
    :else-s-expr nil
    :id 0)))

(def-ast-test if.2
  "(if a 1 2)"
  (list
   (make-instance
    'if-node
    :keyword-atom (make-instance
                   'keyword-atom-node
                   :lexem-info (make-lexem
                                "if"
                                1
                                2
                                :symbol
                                :lex-id 2)
                   :id 1)
    :test-s-expr (make-instance
                  'simple-atom-node
                  :lexem-info (make-lexem
                               "a"
                               1
                               5
                               :symbol
                               :lex-id 3)
                  :id 2)
    :parenthesis-info `((:lparen-coord 1 1)
                        (:rparen-coord 1 10))
    :then-s-expr (make-instance
                  'simple-atom-node
                  :lexem-info (make-lexem
                               "1"
                               1
                               7
                               :integer
                               :lex-id 4)
                  :id 3)
    :else-s-expr (make-instance
                  'simple-atom-node
                  :lexem-info (make-lexem
                               "2"
                               1
                               9
                               :integer
                               :lex-id 5)
                  :id 4)
    :id 0)))
