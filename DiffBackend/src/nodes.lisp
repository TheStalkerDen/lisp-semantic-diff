(uiop:define-package :diff-backend/nodes
    (:nicknames :ast-nodes)
  (:use :cl :diff-backend/utils)
  (:export))

(in-package :diff-backend/nodes)

(defmacro define-node (name
                       (&rest superclasses)
                       (&rest slot-specs)
                       &optional class-option)
  `(progn
     (defclass* ,name (,@(nconc superclasses '(diff-status-mixin)))
       ,slot-specs
       ,(when class-option
          class-option))))

(defclass* diff-status-mixin ()
  ((diff-status :accessor diff-status
                :initarg :diff-status
                :initform :same)))

(defclass* parenthesis-mixin ()
  ((parenthesis-info :accessor parenthesis-info
                     :initarg :parenthesis-info)))

(defclass* keyword-mixin ()
  ((keyword-lexem :accessor keyword-lexem
                  :initarg :keyword-lexem)))

(define-node lexem-wrapper-node ()
  ((lexem-info :accessor lexem-info
               :initarg :lexem-info)))

(define-node defun-node (parenthesis-mixin keyword-mixin)
  ((parameters-list :accessor parameters-list
                    :initarg :parameters-list)
   (documentation-string :accessor documentation-string
                         :initarg :documentation-string)
   (body-forms :accessor body-forms
               :initarg :body-forms)))

(define-node function-call-node (parenthesis-mixin)
  ((func-lexem :accessor func-lexem
               :initarg :func-lexem)
   (func-arg-forms :accessor func-arg-forms
                   :initarg :func-arg-forms)))

(define-node list-node (parenthesis-mixin)
  ((elements :accessor elements
              :initarg :elements)))


