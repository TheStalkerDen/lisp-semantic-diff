(uiop:define-package :diff-backend/nodes
    (:nicknames :ast-nodes)
  (:use :cl :diff-backend/utils
        :diff-backend/lexer)
  (:export #:get-lexem-name
           #:simple-atom-node
           #:parm-atom-node
           #:decl-var-atom-node
           #:func-name-atom-node
           #:keyword-atom-node
           #:q-atom-node
           #:parm-list-node
           #:bindings-list-node
           #:q-list-node))

(in-package :diff-backend/nodes)

(defmacro define-node (name
                       (&rest superclasses)
                       (&rest slot-specs)
                       &optional class-option)
  `(progn
     (defclass* ,name (,@(nconc superclasses '(main-fields-mixin
                                               spec-length-mixin)))
         ,slot-specs
       ,(when class-option
          class-option))))

(defclass* main-fields-mixin ()
    ((diff-status :accessor diff-status
                  :initarg :diff-status
                  :initform :same)
     (id :accessor id
         :initarg :id
         :initform -1)))

(defclass* parenthesis-mixin ()
  ((parenthesis-info :accessor parenthesis-info
                     :initarg :parenthesis-info)))

(defclass* keyword-mixin ()
    ((keyword-atom :accessor keyword-atom
                   :initarg :keyword-atom)))

(defclass* spec-length-mixin ()
  ((spec-length :accessor spec-length
                :initarg :spec-length)))

(defgeneric calculate-spec-length (obj)
  (:method (obj)
    (print "Can't calculate spec-length for obj ~S" obj)))

(defmethod calculate-spec-length :around ((obj spec-length-mixin))
  (if (slot-boundp obj 'spec-length)
      (slot-value obj 'spec-length)
      (call-next-method)))

(defmethod initialize-instance :after ((obj spec-length-mixin) &key)
  (with-slots (spec-length) obj
    (unless (slot-boundp obj 'spec-length)
      (setf spec-length (calculate-spec-length obj)))))

;;;ILLEGAL-S-EXPR
(define-node illegal-node (parenthesis-mixin)
    ((elements :accessor elements
               :initarg :elements)
     (is-top? :accessor top?
             :initarg :is-top?)))

(defmethod calculate-spec-length ((obj illegal-node))
  (values 0))

;;;ATOM-NODE
(define-node atom-node ()
  ((lexem-info :accessor lexem-info
               :initarg :lexem-info)))

(defgeneric get-lexem-name (obj))

(defmethod get-lexem-name ((obj atom-node))
  (slot-value (slot-value obj 'lexem-info) 'string))

(defmethod calculate-spec-length ((obj atom-node))
  (with-slots (lexem-info) obj
    (length (lexem-string lexem-info))))

(defmethod print-object ((obj atom-node) stream)
  (with-slots (lexem-info) obj
      (print-unreadable-object (obj stream)
        (format stream "~S" (lexem-string lexem-info)))))

(defclass simple-atom-node (atom-node) ())

(defclass parm-atom-node (atom-node) ())

(defclass decl-var-atom-node (atom-node) ())

(defclass func-name-atom-node (atom-node) ())

(defclass keyword-atom-node (atom-node) ())

(defclass q-atom-node (atom-node) ())

;;;DEFUN-NODE
(define-node defun-node (parenthesis-mixin keyword-mixin)
  ((function-name :accessor function-name
                  :initarg :func-name)
   (parameters-list :accessor parameters-list
                    :initarg :parameters-list)
   (documentation-string :accessor documentation-string
                         :initarg :documentation-string)
   (body-forms :accessor body-forms
               :initarg :body-forms)))

(defmethod calculate-spec-length ((obj defun-node))
  (with-slots (function-name parameters-list body-forms keyword-atom) obj
    (+ (1+ (calculate-spec-length function-name))
       (1+ (calculate-spec-length parameters-list))
       (1+ (calculate-spec-length body-forms))
       (1+ (calculate-spec-length keyword-atom))
         2)))

;;;FUNCTION-CALL-NODE
(define-node function-call-node (parenthesis-mixin)
  ((func-lexem :accessor func-lexem
               :initarg :func-lexem)
   (func-arg-forms :accessor func-arg-forms
                   :initarg :func-arg-forms)))

(defmethod calculate-spec-length ((obj function-call-node))
  (with-slots (func-lexem func-arg-forms) obj 
    (+ (1+ (calculate-spec-length func-lexem))
       (calculate-spec-length func-arg-forms)
       2)))

(defmethod print-object ((obj function-call-node) stream)
  (with-slots (func-lexem) obj
    (print-unreadable-object (obj stream)
      (format stream "Funcall ~S" func-lexem))))

;;;LIST-NODE
(define-node list-node (parenthesis-mixin)
  ((elements :accessor elements
             :initarg :elements)))

(defclass parm-list-node (list-node) ())

(defclass bindings-list-node (list-node) ())

(defclass q-list-node (list-node) ())

(defmethod calculate-spec-length ((obj list-node))
  (with-slots (elements) obj
    (+ 2 (calculate-spec-length elements))))

(defmethod calculate-spec-length ((obj list))
  (1- (loop :for el :in obj
            :sum (1+ (calculate-spec-length el)))))

(defmethod calculate-spec-length ((obj vector))
  (1- (loop :for el :across obj
            :sum (1+ (calculate-spec-length el)))))

;;;QUOTE-NODE
(define-node quote-node ()
    ((quote-coord :accessor quote-coord
                  :initarg :quote-coord)
     (q-s-expr :accessor q-s-expr
               :initarg :q-s-expr)))

(defmethod calculate-spec-length ((obj quote-node))
  (with-slots (q-s-expr) obj
    (+ 1 (calculate-spec-length q-s-expr))))

;;;LET-NODE
(define-node let-node (parenthesis-mixin keyword-mixin)
    ((bindings :accessor bindings
               :initarg :bindings)
     (body-forms :accessor body-forms
                 :initarg :body-forms)))

(defmethod calculate-spec-length ((obj let-node))
  (with-slots (keyword-atom bindings body-forms)
      obj
    (+ 2
       (1+ (calculate-spec-length keyword-atom))
       (1+ (calculate-spec-length bindings))
       (calculate-spec-length body-forms))))

;;;let-binding-node
(define-node let-binding-node (parenthesis-mixin)
    ((var-atom :accessor var-atom
               :initarg :var-atom)
     (value-s-expr :accessor value-s-expr
                   :initarg :value-s-expr)))

(defmethod calculate-spec-length ((obj let-binding-node))
  (with-slots (var-atom value-s-expr)
      obj
    (+ 2 (1+ (calculate-spec-length var-atom))
       (if value-s-expr
           (calculate-spec-length value-s-expr)
           0))))

;;;defparameter-node
(define-node defparameter-node (parenthesis-mixin keyword-mixin)
  ((parameter-name :accessor parameter-name
                   :initarg :parameter-name)
   (value-s-expr :accessor value-s-expr
                 :initarg :value-s-expr)))

(defmethod calculate-spec-length ((obj defparameter-node))
  (with-slots (keyword-atom parameter-name value-s-expr)
      obj
    (+ 2 (1+ (calculate-spec-length keyword-atom))
       (1+ (calculate-spec-length parameter-name))
       (calculate-spec-length value-s-expr))))

;;;if-node
(define-node if-node (parenthesis-mixin keyword-mixin)
    ((test-s-expr :accessor test-s-expr
                  :initarg :test-s-expr)
     (then-s-expr :accessor then-s-expr
                  :initarg :then-s-expr)
     (else-s-expr :accessor else-s-expr
                  :initarg :else-s-expr)))

(defmethod calculate-spec-length ((obj if-node))
  (with-slots (keyword-atom test-s-expr then-s-expr else-s-expr)
      obj
      (+ 2 (1+ (calculate-spec-length keyword-atom))
         (1+ (calculate-spec-length test-s-expr))
         (1+ (calculate-spec-length then-s-expr))
         (calculate-spec-length else-s-expr))))
