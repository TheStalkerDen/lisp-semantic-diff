(uiop:define-package :diff-backend/comparator
    (:nicknames :comparator)
  (:use :cl :diff-backend/nodes)
  (:export #:start-compare))

(in-package :diff-backend/comparator)

(defparameter *maybe-deleted-nodes* nil)

(defparameter *maybe-new-nodes* nil)

(defparameter *cur-sem-status* nil)

(defun start-compare (obj1 obj2)
  (let ((*maybe-deleted-nodes* nil
         ;(make-hash-table :test 'equal)
         )
        (*maybe-new-nodes* nil
         ;(make-hash-table :test 'equal)
         ))
    (compare obj1 obj2)
    (maybe-issue-resolver)))

;;;is also stupid
(defun maybe-issue-resolver ()
  (dolist (maybe-deleted *maybe-deleted-nodes*)
    (dolist (maybe-new *maybe-new-nodes*)
      (when (compare maybe-deleted maybe-new)
        (set-diff-status maybe-new :moved)
        (set-diff-status maybe-deleted :moved)
        (return)))
    (setf *maybe-new-nodes*
          (remove-if
           (lambda (el)
             (eq (diff-status el) :moved))
           *maybe-new-nodes*)))
  (setf *maybe-deleted-nodes*
        (remove-if
         (lambda (el)
           (eq (diff-status el) :moved))
         *maybe-deleted-nodes*))
  (dolist (del-obj *maybe-deleted-nodes*)
    (set-diff-status del-obj :deleted))
  (dolist (new-obj *maybe-new-nodes*)
    (set-diff-status new-obj :new)))

;;;just to demonstrate that this work
(defgeneric compare (obj1 obj2)
  (:method (obj1 obj2)
    (set-diff-status obj1 :deleted)
    (set-diff-status obj2 :new)))

(defgeneric set-diff-status (obj status)
  (:method (obj1 obj2)
    (format t "set-diff-statud default method. Something is wrong: ~A ~A~%" obj1 obj2)))

(defmethod set-diff-status ((obj diff-status-mixin) status)
  (setf (diff-status obj) status))

;;is super stupid now
;; has a bug...
(defmethod compare ((obj1 list) obj2)
  (if (listp obj2)
      (do* ((rest-obj1 obj1 (rest rest-obj1))
            (rest-obj2 obj2 (rest rest-obj2)))
           ((or (null rest-obj1)
                (null rest-obj2))
            (if rest-obj1
                (mapcar (lambda (el)
                          (set-diff-status el :deleted)
                          (push el *maybe-deleted-nodes*))
                        rest-obj1)
                (mapcar (lambda (el)
                          (set-diff-status el :new)
                          (push el *maybe-new-nodes*))
                        rest-obj2)))
        (unless (compare (first rest-obj1) (first rest-obj2))
          (push (first rest-obj1) *maybe-deleted-nodes*)
          (push (first rest-obj2) *maybe-new-nodes*)))
      (error "something is wrong...")))

(defmethod compare ((obj1 defun-node) obj2)
  (when (and (eq (type-of obj2) 'defun-node)
             (let ((*cur-sem-status* :func-name))
               (compare (function-name obj1)
                        (function-name obj2))))
    (compare (parameters-list obj1)
             (parameters-list obj2))
    (compare (body-forms obj1)
             (body-forms obj2))
    t))

;;;vararg problem
;;;renaming prolem
(defmethod compare ((obj1 function-call-node) obj2)
  (when (and (eq (type-of obj2) 'function-call-node)
             (let ((*cur-sem-status* :func-name))
               (compare (func-lexem obj1)
                        (func-lexem obj2))))
    (compare (func-arg-forms obj1)
             (func-arg-forms obj2))
    t))

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
    (unless (eq *cur-sem-status* :func-name)
      (set-diff-status obj1 :deleted)
      (set-diff-status obj2 :new))
    (return-from compare nil))
  t)
