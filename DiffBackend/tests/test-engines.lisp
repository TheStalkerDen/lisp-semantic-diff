(uiop:define-package :diff-backend/tests/test-engines
    (:use :cl
          :diff-backend/parser
          :diff-backend/lexer
          :diff-backend/nodes
          :diff-backend/abstract-sem-tree-generator
          :rove)
  (:export
   #:def-ast-test
   #:def-parser-test
   #:def-lexer-test))

(in-package :diff-backend/tests/test-engines)

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
     (let ((ast-gen-res (abstract-sem-tree-gen (parser (lexer ,str)))))
       (break)
       (unless (deep-equal ast-gen-res ,obj-tree)
         (fail "AST-gen test failed")))))

(defun deep-equal (obj1 obj2)
  (when (eq (type-of obj1) (type-of obj2))
    (typecase obj1 
      (standard-object
       (loop :for slot :in (closer-mop:class-slots (class-of obj1))
          :for slot-name = (closer-mop:slot-definition-name slot)
          :always (or (and (null (slot-boundp obj1 slot-name))
                           (null (slot-boundp obj2 slot-name)))
                      (deep-equal (slot-value obj1 slot-name)
                                  (slot-value obj2 slot-name)))))
      (t (equalp obj1 obj2)))))
