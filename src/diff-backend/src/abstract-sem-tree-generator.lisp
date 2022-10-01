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

(defvar *current-id*)

(defparameter *semanitc-errors-list* nil)

(defclass semantic-error-info ()
  ((error-text :accessor error-text
               :initarg :error-text)
   (error-node :accessor error-node
               :initarg :error-node)))

(defun abstract-sem-tree-gen (syn-tree &key (curr-file 1))
  (let* ((*current-file-ver* curr-file)
         (*current-id* 0)
         (*semanitc-errors-list*)
         (res-ast (match-top syn-tree)))
    (values res-ast
            (nreverse *semanitc-errors-list*))))

(defun get-id ()
  (prog1 *current-id*
    (incf *current-id*)))

(defun match-top (root-of-syn-tree)
  (destructuring-bind (term-type annotations &rest elements)
      root-of-syn-tree
    (declare (ignore term-type annotations))
    (mapcar (lambda (term-el)
              (match-s-expr term-el))
            elements)))

(defun match-s-expr (term-el)
  (destructuring-bind (term-type annotations first-s-expr &rest others)
      term-el
    (ecase term-type
      ((:atom) (make-atom-node term-el))
      ((:list)
       (unless (eq (get-term-type first-s-expr) :atom)
         (return-from match-s-expr
           (make-ill-node-and-push-to-errors-list
            "Illegal function call"
            term-el)))
       (unless (eq (get-atom-term-type first-s-expr) :symbol)
         (return-from match-s-expr
           (make-ill-node-and-push-to-errors-list
            "Illegal function call"
            term-el)))
       (let ((normal-symbol-string
               (get-symbol-atom-normal-string-form first-s-expr)))
         (cond
           ((string= normal-symbol-string "DEFUN")
            (match-defun first-s-expr annotations others))
           ((string= normal-symbol-string "DEFPARAMETER")
            (match-defparameter first-s-expr annotations others))
           ((string= normal-symbol-string "LET")
            (match-let first-s-expr annotations others))
           ((string= normal-symbol-string "IF")
            (match-if first-s-expr annotations others))
           (t (match-function-call first-s-expr annotations others)))))
      ((:quote)
       (match-quote term-el)))))

(defun make-ill-node-and-push-to-errors-list (error-text ill-term)
  (let ((ill-node (make-illegal-node ill-term :is-top? t))
        (coord (ecase (get-term-type ill-term)
                 ((:atom) (get-atom-term-position ill-term))
                 ((:list) (rest (assoc :lparen-coord (second ill-term)))))))
    (push (make-instance 'semantic-error-info
                         :error-text
                         (format nil "At (~S:~S) ~A"
                                 (first coord)
                                 (second coord)
                                 error-text)
                         :error-node (id ill-node))
          *semanitc-errors-list*)
    ill-node))

(defun is-atom-s-expr? (s-expr)
  (eq (first s-expr) :atom))
(defun get-form*-vector (form*-list)
  (when form*-list
    (map 'vector #'match-s-expr form*-list)))

(defun get-term-type (term)
  (first term))

(defun get-atom-term-type (atom-term)
  (lexer:lexem-type (third atom-term)))

(defun get-atom-term-position (atom-term)
  (let ((lex-info (third atom-term)))
    (list (lexer:lexem-line lex-info)
          (lexer:lexem-column lex-info))))

(defun get-symbol-atom-normal-string-form (atom-term)
  (string-upcase (lexem-string (third atom-term))))

(defun make-atom-node (atom-term &key (atom-type 'simple-atom-node))
  (make-instance atom-type
                 :lexem-info (third atom-term)
                 :id (get-id)))

(defun make-illegal-node (term &key is-top?)
  (let ((node-id (get-id)))
    (destructuring-bind (term-type annotations &rest others)
        term
      (ecase term-type
        ((:atom) (make-atom-node term))
        ((:list) (make-instance 'illegal-node
                                :id node-id
                                :parenthesis-info annotations
                                :elements (map 'vector
                                               #'make-illegal-node
                                               others)
                                :is-top? is-top?))))))

(defun match-defun (keyword-term par-info elements)
  (handler-case
      (destructuring-bind (func-name parms &rest forms)
          elements
        (let* ((node-id (get-id))
               (res-obj
                 (make-instance
                  'defun-node
                  :keyword-atom (make-atom-node keyword-term :atom-type 'keyword-atom-node)
                  :func-name (make-atom-node func-name :atom-type 'func-name-atom-node)
                  :parenthesis-info par-info
                  :parameters-list (make-parm-list-node parms)
                  :body-forms (get-form*-vector forms)
                  :id node-id)))
          (add-to-stats (get-symbol-atom-normal-string-form func-name)
                        res-obj
                        :stat-name :defuns
                        :file-ver *current-file-ver*)
          res-obj))
    (error (c)
      (print c)
      (make-ill-node-and-push-to-errors-list
       "incorrect defun"
       `(:list ,par-info ,keyword-term ,@elements)))))

(defun match-defparameter (keyword-term par-info elements)
  (handler-case
      (destructuring-bind (parameter-name value-s-expr)
          elements
        (let* ((node-id (get-id))
               (res-obj
                 (make-instance
                  'defparameter-node
                  :keyword-atom (make-atom-node keyword-term :atom-type 'keyword-atom-node)
                  :parameter-name (make-atom-node parameter-name :atom-type 'func-name-atom-node)
                  :value-s-expr (match-s-expr value-s-expr)
                  :parenthesis-info par-info
                  :id node-id)))
          (add-to-stats (get-symbol-atom-normal-string-form parameter-name)
                        res-obj
                        :stat-name :defparameters
                        :file-ver *current-file-ver*)
          res-obj))
    (error (c)
      (declare (ignore c))
      (make-ill-node-and-push-to-errors-list
       "incorrect defparameter"
       `(:list ,par-info ,keyword-term ,@elements)))))

(defun match-let (keyword-term par-info elements)
  (handler-case
      (destructuring-bind (let-bindings &rest forms)
          elements
        (let* ((node-id (get-id))
               (res-obj
                 (make-instance
                  'let-node
                  :keyword-atom (make-atom-node keyword-term :atom-type 'keyword-atom-node)
                  :bindings (match-let-bindings let-bindings)
                  :parenthesis-info par-info
                  :body-forms (get-form*-vector forms)
                  :id node-id)))
          res-obj))
    (error (c)
                                        ;(declare (ignore c))
      (print c)
      (make-ill-node-and-push-to-errors-list
       "incorrect let"
       `(:list ,par-info ,keyword-term ,@elements)))))

(defun match-if (keyword-term par-info elements)
  (handler-case
      (destructuring-bind (test-s-expr then-s-expr &optional else-s-expr)
          elements
        (let* ((node-id (get-id))
               (res-obj
                 (make-instance
                  'if-node
                  :keyword-atom (make-atom-node keyword-term :atom-type 'keyword-atom-node)
                  :test-s-expr (match-s-expr test-s-expr)
                  :then-s-expr (match-s-expr then-s-expr)
                  :else-s-expr (when else-s-expr (match-s-expr else-s-expr))
                  :parenthesis-info par-info
                  :id node-id)))
          res-obj))
    (error (c)
                                        ;(declare (ignore c))
      (print c)
      (make-ill-node-and-push-to-errors-list
       "incorrect if"
       `(:list ,par-info ,keyword-term ,@elements)))))

(defun match-let-bindings (bindings-term)
  (destructuring-bind (term-type par-info &rest bindings)
      bindings-term
    (if (eq term-type :list)
        (let ((node-id (get-id)))
          (make-instance
           'bindings-list-node
           :id node-id
           :parenthesis-info par-info
           :elements (when bindings
                       (get-let-bindings-vector
                        bindings))))
        (make-ill-node-and-push-to-errors-list
         "incorrect let bindings"
         bindings-term))))

(defun get-let-bindings-vector (bindings)
  (map 'vector #'match-let-binding bindings))

(defun match-let-binding (binding)
  (destructuring-bind (term-type par-info &rest others)
      binding
    (cond
      ((eq term-type :atom)
       (unless (eq (get-atom-term-type binding)
                   :symbol)
         (return-from match-let-binding
           (make-ill-node-and-push-to-errors-list
            "incorrect let binding (only symbol can be used as local var)"
            binding)))
       (make-instance
        'decl-var-atom-node
        :id (get-id)
        :lexem-info (first others)))
      ((eq term-type :list)
       (let ((len (length others)))
         (unless (or (= len 1)
                     (= len 2))
           (return-from match-let-binding
             (make-ill-node-and-push-to-errors-list
              "malformed let binding"
              binding)))
         (let ((var (first others))
               (value (second others)))
           (unless (eq (get-term-type var) :atom)
             (return-from match-let-binding
               (make-ill-node-and-push-to-errors-list
                "malformed let binding"
                binding)))
           (let ((node-id (get-id)))
             (make-instance
              'let-binding-node
              :id node-id
              :parenthesis-info par-info
              :var-atom (make-atom-node var :atom-type 'decl-var-atom-node)
              :value-s-expr (when value
                              (match-s-expr value))))))))))

(defun make-parm-list-node (list-element)
  (when (eq (first list-element) :list)
    (let ((node-id (get-id)))
      (make-instance 'parm-list-node
                     :parenthesis-info (second list-element)
                     :elements (get-form*-vector (rest (rest list-element)))
                     :id node-id))))

(defun match-function-call (func-name-term par-info func-args)
  (let ((node-id (get-id)))
    (make-instance 'function-call-node
                   :func-lexem (make-atom-node func-name-term :atom-type 'func-name-atom-node)
                   :parenthesis-info par-info
                   :func-arg-forms (get-form*-vector func-args)
                   :id node-id)))

(defun match-quote (term)
  (let ((node-id (get-id)))
    (make-instance 'quote-node
                   :id node-id
                   :quote-coord (rest (first (second term)))
                   :q-s-expr (make-q-data (third term)))))

(defun make-q-data (term)
  (destructuring-bind (type annotations &rest others)
      term
    (ecase type
      ((:atom)
       (make-atom-node term :atom-type 'q-atom-node))
      ((:list)
       (make-q-list annotations others))
      ((:quote)
       (match-quote term)))))

(defun make-q-list (par-info list-elements)
  (let ((node-id (get-id)))
    (make-instance 'q-list-node
                   :id node-id
                   :parenthesis-info par-info
                   :elements (map 'vector #'make-q-data list-elements))))
