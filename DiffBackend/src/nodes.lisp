(uiop:define-package :diff-backend/nodes
    (:nicknames :ast-nodes)
  (:use :cl :diff-backend/utils
        :diff-backend/lexer)
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

(defmethod print-object ((obj lexem-wrapper-node) stream)
  (with-slots (lexem-info) obj
      (print-unreadable-object (obj stream)
        (format stream "~S" (lexem-string lexem-info)))))

(define-node defun-node (parenthesis-mixin keyword-mixin)
  ((function-name :accessor function-name
                  :initarg :func-name)
   (parameters-list :accessor parameters-list
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

(defmethod print-object ((obj function-call-node) stream)
  (with-slots (func-lexem) obj
    (print-unreadable-object (obj stream)
      (format stream "Funcall ~S" func-lexem))))

(define-node list-node (parenthesis-mixin)
  ((elements :accessor elements
              :initarg :elements)))


