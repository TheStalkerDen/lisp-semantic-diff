(uiop:define-package :diff-backend/nodes
    (:nicknames :ast-nodes)
  (:use :cl)
  (:export))

(defclass diff-status-mixin ()
  (diff-status :accessor diff-status
               :initarg :diff-status
               :initform :same))

(defclass parenthesis-mixin ()
  (parenthesis-info :accessor parenthesis-info
                    :initarg :parenthesis-info))

(defclass keyword-mixin ()
  (keyword-lexem :accessor keyword-lexem
                 :initarg :keyword-lexem))

(defclass lexem-wrapper-node (diff-status-mixin)
  (lexem-info :accessor lexem-info
              :initarg :lexem-info))

(defclass defun-node (parenthesis-mixin keyword-mixin diff-status-mixin)
  (parameters-list :accessor parameters-list
                   :initarg :parameters-list)
  (documentation-string :accessor documentation-string
                        :initarg :documentation-string)
  (body-forms :accessor body-forms
              :initarg :body-forms))

(defclass function-call-node (parenthesis-mixin diff-status-mixin)
  (func-lexem :accessor func-lexem
              :initarg :func-lexem)
  (func-arg-forms :accessor func-arg-forms
                  :initarg :func-arg-forms))

(defclass list-node (parenthesis-mixin diff-status-mixin)
  (elements :accessor elements
            :initarg :elements))


