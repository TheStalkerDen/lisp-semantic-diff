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

(def-simple-classifier-test only-no-mod-defuns.4
    "(defun a (k a) 'k 'a)"
  "(defun a (k a) 'k   'a)"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test only-no-mod-defuns.5
    "(defun a (k a) '(k) '(a))"
  "(defun a (k a) '(k)   '(a))"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test only-no-mod-defuns.6
    "(defun a (k a)
       (let ((a 1))
         a))"
  "(defun a (k a) (let ((a 1)) a))"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test only-no-mod-defuns.7
    "(defun a (k a)
       (if (a 1)
         a))"
  "(defun a (k a) (if (a 1) a))"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test only-no-mod-defuns.8
    "(defun a (k a)
       (if (a 1)
         a 2))"
  "(defun a (k a) (if (a 1) a 2))"
  `((:defuns
        (:no-mod
         ("a"))))
  `((:defuns
        (:no-mod
         ("a")))))

(def-simple-classifier-test only-no-mod-defparameters.1
  "(defparameter a 1)"
  "(defparameter a 1)"
  `((:defparameters
        (:no-mod
         ("a"))))
  `((:defparameters
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

(def-simple-classifier-test mod-defuns.2
    "(defun a () '1)"
  "(defun a () 1)"
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

(def-comparator-test cmp.test.simple-defun-body.1
  "(defun a () 1)"
  "(defun a () 2)"
  (list
   `(:<el> :<el> () (:deleted :<el>)))
  (list
   `(:<el> :<el> () (:new :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.simple-defun-body.2
  "(defun a () 1 2)"
  "(defun a () 2)"
  (list
   `(:<el> :<el> () (:deleted :<el>) :<el>))
  (list
   `(:<el> :<el> () :<el>))
  :simple-form t)

(def-comparator-test cmp.test.simple-defun-body.3
  "(defun a () 1 2 2)"
  "(defun a () 2 2)"
  (list
   `(:<el> :<el> () (:deleted :<el>) :<el> :<el>))
  (list
   `(:<el> :<el> () :<el> :<el>))
  :simple-form t)

(def-comparator-test cmp.test.simple-defun-body.4
  "(defun a () 2 2 1)"
  "(defun a () 2 2)"
  (list
   `(:<el> :<el> () :<el> :<el> (:deleted :<el>)))
  (list
   `(:<el> :<el> () :<el> :<el>))
  :simple-form t)

(def-comparator-test cmp.test.simple-defun-body.4.2
  "(defun a () 2 2 6 3)"
  "(defun a () 2 2 6 1 1)"
  (list
   `(:<el> :<el> () :<el> :<el> :<el> (:deleted :<el>)
           ))
  (list
   `(:<el> :<el> () :<el> :<el> :<el> (:new :<el>) (:new :<el>)))
  :simple-form t)

;;;incorrect
(def-comparator-test cmp.test.simple-defun-body.5
  "(defun a () 1 2)"
  "(defun a () 2 1)"
  (list
   `(:<el> :<el> () :<el> ((:moved 4) :<el>)))
  (list
   `(:<el> :<el> () ((:moved 5) :<el>) :<el>))
  :simple-form t)

;;incorrect
(def-comparator-test cmp.test.simple-defun-body.6
  "(defun a () 1 2 3)"
  "(defun a () 2 1 3)"
  (list
   `(:<el> :<el> () :<el> ((:moved 4) :<el>) :<el>))
  (list
   `(:<el> :<el> () ((:moved 5) :<el>) :<el> :<el>))
  :simple-form t)

(def-comparator-test cmp.test.simple-defun-body.7
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
   `(:<el> :<el> (:<el> :<el>)
           (:deleted :<el> ((:moved 8) :<el>) ((:moved 9) :<el>) :<el>)))
  (list
   `(:<el> :<el> (:<el> :<el>)
           (:new  :<el> ((:moved 8) :<el>) ((:moved 9) :<el>) :<el>)))
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
   `(:<el> :<el> () (:deleted :<el> :<el> :<el>) (:<el> :<el> :<el>)))
  (list
   `(:<el> :<el> () (:<el> :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.10
  "(defun f1 () (min 1 2) (ren 23) (add 1 2))"
  "(defun f1 () (ren 23) (gen 12) (add 1 2))"
  (list
   `(:<el> :<el> () (:deleted :<el> :<el> :<el>) ( :<el> :<el>) (:<el> :<el> :<el>)))
  (list
   `(:<el> :<el> () (:<el> :<el>) (:new :<el> :<el>) (:<el> :<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.11
  "(defun f1 () (gen 12))"
  "(defun f1 () (ren 23) (gen 12) (gen 12))"
  (list
   `(:<el> :<el> () (:<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> :<el>) (:new :<el> :<el>) (:<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.wrapper.1
  "(defun f1 () (gen 12))"
  "(defun f1 () (wrapper (gen 12)))"
  (list
   `(:<el> :<el> () ((:moved 6) :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> ((:moved 4) :<el> :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.wrapper.2
  "(defun f1 () (gen 12))"
  "(defun f1 () (wrapper (gen 12) (f 12)))"
  (list
   `(:<el> :<el> () ((:moved 6) :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> ((:moved 4) :<el> :<el>) (:<el> :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.wrapper.3
  "(defun f1 () (gen 12))"
  "(defun f1 () (wrapper (gen 12) (gen 12) (f 12)))"
  (list
   `(:<el> :<el> () ((:moved 6) :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> ((:moved 4) :<el> :<el>) (:<el> :<el>) (:<el> :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.wrapper.4
  "(defun f1 () 
     (gen 12))"
  "(defun f1 () 
    (wrapper1
     (wrapper2
       (gen 12))))"
  (list
   `(:<el> :<el> () ((:moved 8) :<el> :<el>)))
  (list
   `(:<el> :<el> () (:new :<el> (:<el> ((:moved 4) :<el> :<el>)))))
  :simple-form t)

(def-comparator-test cmp.test.15
  "(defun f1 () (gen 12) (f (g 1)))"
  "(defun f1 () (g 1) (gen 12))"
  (list
   `(:<el> :<el> () (:<el> :<el>) (:deleted :<el> ((:moved 4) :<el> :<el>))))
  (list
   `(:<el> :<el> () ((:moved 9) :<el> :<el>) (:<el> :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.16
  "(defun f1 () (gen 12) (f (g 1)))"
  "(defun f1 () (g 1) (gen 12) (g 1))"
  (list
   `(:<el> :<el> () (:<el> :<el>) (:deleted :<el> ((:moved 10) :<el> :<el>))))
  (list
   `(:<EL> :<EL> NIL (:NEW :<EL> :<EL>) (:<EL> :<EL>) ((:MOVED 9) :<EL> :<EL>)))
  :simple-form t)

(def-comparator-test cmp.test.17
  "(defun f1 () (gen 12) (f (g 1)) (g 1))"
  "(defun f1 () (g 1) (gen 12) (g 1))"
  (list
   `(:<el> :<el> () (:<el> :<el>) (:deleted :<el> ((:moved 4) :<el> :<el>)) (:<el> :<el>)))
  (list
   `(:<EL> :<EL> NIL ((:MOVED 9) :<EL> :<EL>) (:<EL> :<EL>) (:<EL> :<EL>)))
  :simple-form t)

(def-comparator-test cmp.test.18
  "(defun f1 () (gen 12) (f (g 1) (g 1)) (g 1))"
  "(defun f1 () (g 1) (gen 12) (g 1))"
  (list
   `(:<EL> :<EL> NIL (:<EL> :<EL>)
           (:DELETED :<EL> ((:MOVED 4) :<EL> :<EL>) (:<EL> (:DELETED :<EL>)))
           (:<EL> :<EL>)))
  (list
   `(:<EL> :<EL> NIL ((:MOVED 9) :<EL> :<EL>) (:<EL> :<EL>) (:<EL> :<EL>)))
  :simple-form t)

(def-comparator-test cmp.test.let.1
  "(defun f1 () (let ((a 2)) a))"
  "(defun f1 () (let ((a 2)) b))"
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>)) (:deleted :<el>))))
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>)) (:new :<el>))))
  :simple-form t)

(def-comparator-test cmp.test.let.2
  "(defun f1 () (let ((a 2)) a))"
  "(defun f1 () (let ((a 2) (b 3)) a))"
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>)) :<el>)))
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>) (:new :<el> :<el>)) :<el>)))
  :simple-form t)

;;;;;

(def-comparator-test cmp.test.let.3
  "(defun f1 () (let ((a 2)) a))"
  "(defun f1 () (let ((a 2) (b 3)) a))"
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>)) :<el>)))
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>) (:new :<el> :<el>)) :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.let.4
  "(defun f1 () (let ((a 2)) a))"
  "(defun f1 () (let ((a 2) (b 3)) a))"
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>)) :<el>)))
  (list
   `(:<el> :<el> () (:<el> ((:<el> :<el>) (:new :<el> :<el>)) :<el>)))
  :simple-form t)

(def-comparator-test cmp.test.if
  "(defun f1 () (if (a 2) a))"
  "(defun f1 () (if (a 2) (b 3) a))"
  (list
   `(:<el> :<el> () ((:<el> :<el>) ((:moved 12) :<el>))))
  (list
   `(:<el> :<el> () ((:<el> :<el>) (:new :<el> :<el>) ((:moved 9) :<el>))))
  :simple-form t)
