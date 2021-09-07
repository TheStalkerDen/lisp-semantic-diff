(uiop:define-package :diff-backend/tests/test-engines
    (:use :cl
          :diff-backend/parser
          :diff-backend/lexer
          :diff-backend/nodes
          :diff-backend/abstract-sem-tree-generator
          :diff-backend/statistics
          :diff-backend/comparator
          :diff-backend/tests/test-utils
          :rove)
  (:export
   #:def-ast-test
   #:def-parser-test
   #:def-lexer-test
   #:def-stats-test
   #:def-simple-classifier-test
   #:def-comparator-test))

(in-package :diff-backend/tests/test-engines)

(declaim (optimize (debug 3)))

(setf rove:*debug-on-error* t)

(defmacro def-lexer-test (name str lexems-list
                          &key
                            exp-comments
                            exp-lex-errors)
  `(deftest ,name
     (multiple-value-bind (res-lexems res-comments res-lex-errors)
         (lexer ,str)
       (declare (optimize (debug 3)))
       (assert (= (length res-lexems) (length ,lexems-list)))
       (loop
         :for res-lexem :in res-lexems
         :for exp-lemem :in ,lexems-list
         :do (unless (equal-lexem? res-lexem exp-lemem)
               (fail (format nil
                             "Lexem ~S != ~S"
                             res-lexem
                             exp-lemem))))
       (when (or ,exp-comments res-comments)
         (unless (deep-equal (sort (alexandria:hash-table-alist res-comments)
                                 #'< :key #'first)
                           ,exp-comments)
           (fail "Comments not equal!")))
       (when (or ,exp-lex-errors res-lex-errors)
         (unless (deep-equal ,exp-lex-errors res-lex-errors)
           (fail "Errors not equal"))))))

(defmacro def-parser-test (name str parser-exp &key exp-parser-error)
  `(deftest ,name
     (multiple-value-bind (parser-res parser-error)
         (parser (lexer ,str))
       (when (or ,parser-exp parser-res)
         (unless
             (tree-equal
              parser-res
              ,parser-exp
              :test (lambda (x y)
                      (typecase x
                        (lexem (when (typep y 'lexem)
                                 (equal-lexem? x y)))
                        (t (eq x y)))))
           (fail "Parser-test failed")))
       (when (or ,exp-parser-error parser-error)
         (unless (deep-equal ,exp-parser-error parser-error)
           (fail "Errors not equal"))))))

(defmacro def-ast-test (name str obj-tree)
  `(deftest ,name
     (init-stats)
     (let ((ast-gen-res (abstract-sem-tree-gen (parser (lexer ,str)))))
       (unless (deep-equal ast-gen-res ,obj-tree)
         (fail "AST-gen test failed")))))

(defmacro def-stats-test (name str exp)
  `(deftest ,name
     (init-stats)
     (let ((ast-gen-res (abstract-sem-tree-gen (parser (lexer ,str)))))
       (declare (ignore ast-gen-res))
       (let ((res
              (loop :for value :being :the :hash-value :in (get-stats 1)
                 :using (hash-key stat-name)
                 :collect `(,stat-name
                            ,(loop :for name :being :the :hash-key :in value
                                :collect name)))))
         (unless (deep-equal res ,exp)
           (fail "STATS test failed!"))))))

(defmacro def-simple-classifier-test (name str1 str2 exp1 exp2)
  `(deftest ,name
     (init-stats)
     (let ((ast1 (abstract-sem-tree-gen (parser (lexer ,str1)) :curr-file 1))
           (ast2 (abstract-sem-tree-gen (parser (lexer ,str2)) :curr-file 2)))
       (multiple-value-bind (m-ast-1 m-ast-2)
           (start-asts-comparing ast1 ast2)
         (declare (ignore m-ast-1 m-ast-2))
         (let ((res1 (gen-classified-results 1))
               (res2 (gen-classified-results 2)))
           (unless (and (deep-equal res1 ,exp1)
                        (deep-equal res2 ,exp2))
             (fail "Simple classified-test failed")))))))

(defmacro def-comparator-test (name str1 str2 exp1 exp2 &key simple-form)
  `(deftest ,name
     (init-stats)
     (let ((ast1 (abstract-sem-tree-gen (parser (lexer ,str1)) :curr-file 1))
           (ast2 (abstract-sem-tree-gen (parser (lexer ,str2)) :curr-file 2)))
       (multiple-value-bind (m-ast-1 m-ast-2)
           (start-asts-comparing ast1 ast2)
         (unless (and (deep-equal (if ,simple-form
                                      (conv-for-cmp-test m-ast-1)
                                      m-ast-1)
                                  ,exp1)
                      (deep-equal (if ,simple-form
                                      (conv-for-cmp-test m-ast-2)
                                      m-ast-2)
                                  ,exp2))
           (fail "Comparator test failed"))))))
