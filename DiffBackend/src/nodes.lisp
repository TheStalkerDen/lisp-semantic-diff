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
     (defclass* ,name (,@(nconc superclasses '(diff-status-mixin no-whitespace-length-mixin)))
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

(defclass* no-whitespace-length-mixin ()
  ((no-whitespace-length :accessor no-whitespace-length
                         :initarg :no-whitespace-length)))

(defgeneric calculate-no-whitespace-length (obj)
  (:method (obj)
    (print "No no")))

(defmethod calculate-no-whitespace-length :around ((obj no-whitespace-length-mixin))
  (if (slot-boundp obj 'no-whitespace-length)
      (slot-value obj 'no-whitespace-length)
      (call-next-method)))

(defmethod initialize-instance :after ((obj no-whitespace-length-mixin) &key)
  (with-slots (no-whitespace-length) obj
    (unless (slot-boundp obj 'no-whitespace-length)
      (setf no-whitespace-length (calculate-no-whitespace-length obj)))))

(define-node lexem-wrapper-node ()
  ((lexem-info :accessor lexem-info
               :initarg :lexem-info)))

(defmethod calculate-no-whitespace-length ((obj lexem-wrapper-node))
  (with-slots (lexem-info) obj
    (length (lexem-string lexem-info))))

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

(defmethod calculate-no-whitespace-length ((obj defun-node))
  (with-slots (function-name parameters-list body-forms keyword-lexem) obj
      (+ (calculate-no-whitespace-length function-name)
         (calculate-no-whitespace-length parameters-list)
         (calculate-no-whitespace-length body-forms)
         (calculate-no-whitespace-length keyword-lexem)
         2)))

(define-node function-call-node (parenthesis-mixin)
  ((func-lexem :accessor func-lexem
               :initarg :func-lexem)
   (func-arg-forms :accessor func-arg-forms
                   :initarg :func-arg-forms)))

(defmethod calculate-no-whitespace-length ((obj function-call-node))
  (with-slots (func-lexem func-arg-forms) obj 
    (+ (calculate-no-whitespace-length func-lexem)
       (calculate-no-whitespace-length func-arg-forms)
       2)))

(defmethod print-object ((obj function-call-node) stream)
  (with-slots (func-lexem) obj
    (print-unreadable-object (obj stream)
      (format stream "Funcall ~S" func-lexem))))

(define-node list-node (parenthesis-mixin)
  ((elements :accessor elements
             :initarg :elements)))

(defmethod calculate-no-whitespace-length ((obj list-node))
  (with-slots (elements) obj
    (+ 2 (loop :for el :in elements
         :sum (calculate-no-whitespace-length el)))))

(defmethod calculate-no-whitespace-length ((obj list))
  (loop :for el :in obj
       :sum (calculate-no-whitespace-length el)))

