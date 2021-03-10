(uiop:define-package :diff-backend/tests/comparator
    (:use :cl
     :diff-backend/lexer
          :diff-backend/nodes
          :diff-backend/tests/test-utils
          :diff-backend/tests/test-engines))

(in-package :diff-backend/tests/comparator)

(def-simple-classifier-test only-no-mod-defuns.1
    "(defun a () 1)"
  "(defun a () 1)"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test only-no-mod-defuns.2
    "(defun a () 1)
     (defun b () 2)"
  "(defun b () 2)
   (defun a () 1)"
  `((:defuns
        (:no-mod
         ("a" "b"))))
  `((:defuns
        (:no-mod
         ("b" "a")))))

(def-simple-classifier-test only-no-mod-defuns.3
    "(defun a (k a) k a)"
  "(defun a (k a) k   a)"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test completely-different.1
    "(defun b () 1)"
  "(defun a () 1)"
  `((:defuns
        (:deleted
         ("b"))))
  `((:defuns
        (:new
         ("a")))))

(def-simple-classifier-test mod-defuns.1
    "(defun a () 1)"
  "(defun a () 4)"
  `((:defuns
        (:modified
         ("a"))))
  `((:defuns
        (:modified
         ("a")))))

(def-simple-classifier-test mixed.1
    "(defun a () 1)
     (defun k () 9)"
  "(defun a () 4)"
  `((:defuns
        (:modified
         ("a"))
        (:deleted
         ("k"))))
  `((:defuns
        (:modified
         ("a")))))

(def-simple-classifier-test mixed.2
    "(defun a (a) 1)
     (defun k () 9)"
  "(defun a (a) 4)"
  `((:defuns
        (:modified
         ("a"))
        (:deleted
         ("k"))))
  `((:defuns
        (:modified
         ("a")))))

(def-comparator-test cmp.test.1
  "(defun a () 1)"
  "(defun a () 2)"
  (list
   `(:<el> :<el> () (:deleted :<el>)))
  (list
   `(:<el> :<el> () (:new :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.2
  "(defun a () 1 2)"
  "(defun a () 2)"
  (list
   `(:<el> :<el> () (:deleted :<el>) (:moved :<el>)))
  (list
   `(:<el> :<el> () (:moved :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.3
  "(defun a () 1 2)"
  "(defun a () 2 1)"
  (list
   `(:<el> :<el> () (:moved :<el>) (:moved :<el>)))
  (list
   `(:<el> :<el> () (:moved :<el>) (:moved :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.4
  "(defun a () 1 2 3)"
  "(defun a () 2 1 3)"
  (list
   `(:<el> :<el> () (:moved :<el>) (:moved :<el>) :<el>))
  (list
   `(:<el> :<el> () (:moved :<el>) (:moved :<el>) :<el>))
  :simple-form t)

(def-comparator-test cmp.test.5
  "(defun a () 1 2 3)"
  "(defun a () 1 1 3)"
  (list
   `(:<el> :<el> () :<el> (:deleted :<el>) :<el>))
  (list
   `(:<el> :<el> () :<el> (:new :<el>) :<el>))
  :simple-form t)

(def-comparator-test cmp.test.6
  "(defun f1 (a b) (add a b 1))"
  "(defun f1 (a b) (add a b 2))"
  (list
   `(:<el> :<el> (:<el> :<el>) (:<el> :<el> :<el> (:deleted :<el>))))
  (list
   `(:<el> :<el> (:<el> :<el>) (:<el> :<el> :<el> (:new :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.7
  "(defun f1 (a b) (add1 a b 1))"
  "(defun f1 (a b) (add a b 2))"
  (list
   `(:<el> :<el> (:<el> :<el>) (:deleted :<el> :<el> :<el> :<el>)))
  (list
   `(:<el> :<el> (:<el> :<el>) (:new :<el> :<el> :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.8
  "(defun f1 (a b) (add a b 1))"
  "(defun f1 (a c) (add a c 1))"
  (list
   `(:<el> :<el> (:<el> (:deleted :<el>)) (:<el> :<el> (:deleted :<el>) :<el>)))
  (list
   `(:<el> :<el> (:<el> (:new :<el>)) (:<el> :<el>  (:new :<el>) :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.9
  "(defun f1 () (min 1 2) (add 1 2))"
  "(defun f1 () (add 1 2))"
  (list
   `(:<el> :<el> () (:deleted :<el> :<el> :<el>) (:moved :<el> :<el> :<el>)))
  (list
   `(:<el> :<el> () (:moved :<el> :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.10
  "(defun f1 () (min 1 2) (ren 23) (add 1 2))"
  "(defun f1 () (ren 23) (gen 12) (add 1 2))"
  (list
   `(:<el> :<el> () (:deleted :<el> :<el> :<el>) (:moved :<el> :<el>) (:<el> :<el> :<el>)))
  (list
   `(:<el> :<el> () (:moved :<el> :<el>) (:new :<el> :<el>) (:<el> :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.11
  "(defun f1 () (gen 12))"
  "(defun f1 () (ren 23) (gen 12) (gen 12))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> :<el>) (:moved :<el> :<el>) (:new :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.12
  "(defun f1 () (gen 12))"
  "(defun f1 () (wrapper (gen 12)))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> (:moved :<el> :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.13
  "(defun f1 () (gen 12))"
  "(defun f1 () (wrapper (gen 12) (f 12)))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> (:moved :<el> :<el>) (:new :<el> :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.14
  "(defun f1 () (gen 12))"
  "(defun f1 () (wrapper (gen 12) (gen 12) (f 12)))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> (:moved :<el> :<el>) (:new :<el> :<el>) (:new :<el> :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.15
  "(defun f1 () (gen 12) (f (g 1)))"
  "(defun f1 () (g 1) (gen 12))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>) (:deleted (:moved :<el> :<el>))))
  (list
   `(:<el> :<el> () (:moved :<el> :<el>) (:moved :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.16
  "(defun f1 () (gen 12) (f (g 1)))"
  "(defun f1 () (g 1) (g 1) (gen 12))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>) (:deleted (:moved :<el> :<el>))))
  (list
   `(:<el> :<el> () (:new :<el> :<el>) (:moved :<el> :<el>) (:moved :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.17
  "(defun f1 () (gen 12) (f (g 1)))"
  "(defun f1 () (g 1) (gen 12) (g 1))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>) (:deleted (:moved :<el> :<el>))))
  (list
   `(:<el> :<el> () (:new :<el> :<el>) (:moved :<el> :<el>) (:moved :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.18
  "(defun f1 () (gen 12) (f (g 1)) (g 1))"
  "(defun f1 () (g 1) (gen 12) (g 1))"
  (list
   `(:<el> :<el> () (:moved :<el> :<el>) (:deleted (:moved :<el> :<el>)) (:<el> :<el>)))
  (list
   `(:<el> :<el> () (:moved :<el> :<el>) (:moved :<el> :<el>) (:<el> :<el)))
  :simple-form t)
