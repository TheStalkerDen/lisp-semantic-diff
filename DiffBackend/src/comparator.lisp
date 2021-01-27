(uiop:define-package :diff-backend/comparator
    (:nicknames :comparator)
  (:use :cl :diff-backend/nodes)
  (:export #:compare))

(in-package :diff-backend/comparator)

;;;just to demonstrate that this work
(defgeneric compare (obj1 obj2)
  (:method (obj1 obj2)
    (set-diff-status obj1 :deleted)
    (set-diff-status obj2 :new)))

(defgeneric set-diff-status (obj status)
  (:method (obj1 obj2)
      (print "Do nothing now! Sorry")))

(defmethod set-diff-status ((obj diff-status-mixin) status)
  (setf (diff-status obj) status))

;;is super stupid now
;; has a bug...
(defmethod compare ((obj1 list) obj2)
  (if (listp obj2)
      (do* ((rest-obj1 obj1 (rest obj1))
            (rest-obj2 obj2 (rest obj2)))
           ((or (null rest-obj1)
                (null rest-obj2))
            (if rest-obj1
                (mapcar (lambda (el)
                          (set-diff-status el :deleted))
                        rest-obj1)
                (mapcar (lambda (el)
                          (set-diff-status el :new))
                        rest-obj2)))
        (compare (first rest-obj1) (first rest-obj2)))
      (error "something is wrong...")))

(defmethod compare ((obj1 defun-node) obj2)
  (if (eq (type-of obj2) 'defun-node)
      (progn
        (compare (function-name obj1)
                 (function-name obj2))
        (compare (parameters-list obj1)
                 (parameters-list obj2))
        (compare (body-forms obj1)
                 (body-forms obj2)))
      (progn
        (set-diff-status obj1 :deleted)
        (set-diff-status obj2 :new))))

(defmethod compare ((obj1 function-call-node) obj2)
  (if (eq (type-of obj2) 'function-call-node)
      (progn
        (compare (func-lexem obj1)
                 (func-lexem obj2))
        (compare (func-arg-forms obj1)
                 (func-arg-forms obj2)))
      (progn
        (set-diff-status obj1 :deleted)
        (set-diff-status obj2 :new))))

(defmethod compare ((obj1 list-node) obj2)
  (if (eq (type-of obj2) 'list-node)
      (compare (elements obj1)
               (elements obj2))
      (progn
        (set-diff-status obj1 :deleted)
        (set-diff-status obj2 :new))))

;;; is also stupid
(defmethod compare ((obj1 lexem-wrapper-node) obj2)
  (unless (and (eq (type-of obj2) 'lexem-wrapper-node)
               (string= (lexer:lexem-string (lexem-info obj1))
                        (lexer:lexem-string (lexem-info obj2))))
    (set-diff-status obj1 :deleted)
    (set-diff-status obj2 :new)))
