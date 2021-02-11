(uiop:define-package :diff-backend/abstract-sem-tree-generator
    (:nicknames :ast-gen)
  (:use :cl :diff-backend/nodes
        :diff-backend/statistics)
  (:import-from :diff-backend/lexer
                :is-lexem-symbol?=
                :lexem-string)
  (:export #:abstract-sem-tree-gen))

(in-package :diff-backend/abstract-sem-tree-generator)

(declaim (optimize safety))

(defparameter *current-file-ver* 1)

;;; ast = abstract syntax tree
(defun abstract-sem-tree-gen (ast)
  (top-level-rule ast))

(defun top-level-rule (ast)
  (destructuring-bind (term-type annotations &rest elements)
      ast
    (declare (ignore term-type annotations))
    (mapcar (lambda (el)
              (match-s-expr el))
            elements)))

(defun match-s-expr (el)
  (ecase (first el)
    ((:atom) (make-lexem-wrapper (third el)))
    ((:list)
     (or (match-defun el)
         (match-function-call el)))))

(defun is-atom-s-expr? (s-expr)
  (eq (first s-expr) :atom))

(defun make-lexem-wrapper (lexem)
  (make-instance 'lexem-wrapper-node :lexem-info lexem))

(defun match-defun (list-element)
  (let ((thrd (third list-element)))
    (when (and (is-atom-s-expr? thrd)
               (is-lexem-symbol?= (third thrd) "defun")
               (>= (length list-element) 6))
      (destructuring-bind (type par-info keyword name parms &rest forms)
          list-element
        (declare (ignore type))
        (let ((res-obj
               (make-instance
                'defun-node
                :keyword-lexem (make-lexem-wrapper (third keyword))
                :func-name (make-lexem-wrapper (third name))
                :parenthesis-info par-info
                :parameters-list (gen-list-node parms)
                :body-forms (mapcar (lambda (form)
                                      (match-s-expr form))
                                    forms))))
          (add-to-stats (lexem-string (third keyword))
                        res-obj
                        :stat-name :defuns
                        :file-ver *current-file-ver*)
          res-obj)))))

(defun gen-list-node (list-element)
  (when (eq (first list-element) :list)
    (make-instance 'list-node
                   :parenthesis-info (second list-element)
                   :elements (mapcar (lambda (el)
                                       (match-s-expr el))
                                     (rest
                                      (rest list-element))))))

(defun match-function-call (list-element)
  (destructuring-bind (type par-info func-form &rest args)
      list-element
    (declare (ignore type))
    (make-instance 'function-call-node
                   :func-lexem (match-s-expr func-form)
                   :parenthesis-info par-info
                   :func-arg-forms (mapcar (lambda (arg)
                                             (match-s-expr arg))
                                           args))))
