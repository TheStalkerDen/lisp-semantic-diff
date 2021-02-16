(uiop:define-package :diff-backend/tests/test-engines
    (:use :cl
          :diff-backend/parser
          :diff-backend/lexer
          :diff-backend/nodes
          :diff-backend/abstract-sem-tree-generator
          :diff-backend/statistics
          :diff-backend/comparator
          :rove)
  (:export
   #:def-ast-test
   #:def-parser-test
   #:def-lexer-test
   #:def-stats-test
   #:def-simple-classifier-test))

(in-package :diff-backend/tests/test-engines)

(declaim (optimize (debug 3)))

(defmacro def-lexer-test (name str lexems-list)
  `(deftest ,name
     (let ((res (lexer ,str)))
       (assert (= (length res) (length ,lexems-list)))
       (loop
          :for res-lexem :in res
          :for exp-lemem :in ,lexems-list
          :do (unless (equal-lexem? res-lexem exp-lemem)
                (fail "BAD!"))))))

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

(defmacro def-ast-test (name str obj-tree)
  `(deftest ,name
     (init-stats)
     (let ((ast-gen-res (abstract-sem-tree-gen (parser (lexer ,str)))))
       (unless (deep-equal ast-gen-res ,obj-tree)
         (fail "AST-gen test failed")))))

(defun deep-equal (obj1 obj2)
  (if (equal (type-of obj1) (type-of obj2))
      (if (typecase obj1 
                (standard-object
                 (loop :for slot :in (closer-mop:class-slots (class-of obj1))
                    :for slot-name = (closer-mop:slot-definition-name slot)
                    :always (or (and (null (slot-boundp obj1 slot-name))
                                     (null (slot-boundp obj2 slot-name)))
                                (if (deep-equal (slot-value obj1 slot-name)
                                                (slot-value obj2 slot-name))
                                    t
                                    (progn (print obj1)
                                           (print obj2)
                                           nil)))))
                (list (every #'deep-equal obj1 obj2))
                (t (unless (equalp obj1 obj2)
                     (print obj1)
                     (print obj2)
                     (return-from deep-equal nil))
                   t))
          t
          (progn
            (print obj1)
            (print obj2)
            nil))
    (progn (print "Not equal types")
           (format t "Type: ~A obj1: ~A ~%" (type-of obj1) obj1)
           (format t "Type: ~A obj2: ~A ~%" (type-of obj2) obj2)
           nil)))

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
       (declare (ignore ast1 ast2))
       (compare-results)
       (let ((res1 (gen-classified-results 1))
             (res2 (gen-classified-results 2)))
         (unless (and (deep-equal res1 ,exp1)
                      (deep-equal res2 ,exp2))
           (fail "Simple classified-test failed"))))))
