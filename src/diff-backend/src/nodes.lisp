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

(defvar *2^64* (expt 2 64))

(defun do-hash-math (obj i)
  (mod (* (calculate-hash obj) (expt 31 i)) *2^64*))

(defmacro define-node (name
                       (&rest superclasses)
                       (&rest slot-specs)
                       &optional class-option)
  `(progn
     (defclass* ,name (,@(append superclasses '(main-fields-mixin
                                                spec-length-mixin
                                                hash-mixin
                                                node-counts-mixin)))
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

(defclass* hash-mixin ()
  ((hash :accessor hash
         :initarg :hash)))

(defclass* node-counts-mixin ()
  ((node-counts :accessor node-counts
                :initarg :node-counts)))

(defgeneric calculate-spec-length (obj)
  (:method (obj)
    (print "Can't calculate spec-length for obj ~S" obj)))

(defgeneric calculate-hash (obj)
  (:method (obj)
    (print "Can't calculate hash for obj ~S" obj)))

(defgeneric calculate-node-counts (obj)
  (:method (obj)
    (print "Cant' calculate node-counts for obj ~S" obj)))

(defmethod calculate-spec-length :around ((obj spec-length-mixin))
  (if (slot-boundp obj 'spec-length)
      (slot-value obj 'spec-length)
      (call-next-method)))

(defmethod calcualate-hash :around ((obj hash-mixin))
  (if (slot-boundp obj 'hash-mixin)
      (slot-value obj 'hash-mixin)
      (call-next-method)))

(defmethod calculate-node-counts :around ((obj node-counts-mixin))
  (if (slot-boundp obj 'node-counts)
      (slot-value obj 'node-counts)
      (call-next-method)))

(defmethod initialize-instance :after ((obj spec-length-mixin) &key)
  (with-slots (spec-length) obj
    (unless (slot-boundp obj 'spec-length)
      (setf spec-length (calculate-spec-length obj)))))

(defmethod initialize-instance :after ((obj hash-mixin) &key)
  (with-slots (hash) obj
    (unless (slot-boundp obj 'hash)
      (setf hash (calculate-hash obj)))))

(defmethod initialize-instance :after ((obj node-counts-mixin) &key)
  (with-slots (node-counts) obj
    (unless (slot-boundp obj 'node-counts)
      (setf node-counts (calculate-node-counts obj)))))

(defmethod calculate-hash ((obj symbol))
  (sxhash obj))

;;;ILLEGAL-S-EXPR
(define-node illegal-node (parenthesis-mixin)
    ((elements :accessor elements
               :initarg :elements)
     (is-top? :accessor top?
             :initarg :is-top?)))

(defmethod calculate-spec-length ((obj illegal-node))
  (values 0))

(defmethod calculate-hash ((obj illegal-node))
  (values 0))

(defmethod calculate-node-counts ((obj illegal-node))
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

(defmethod calculate-hash ((obj atom-node))
  (with-slots (lexem-info) obj
    (sxhash (lexem-string lexem-info))))

(defmethod calculate-node-counts ((obj atom-node))
  (values 1))

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

(defmethod calculate-hash ((obj defun-node))
  (with-slots (function-name parameters-list body-forms keyword-atom) obj
    (+ (do-hash-math function-name 0)
       (do-hash-math parameters-list 1)
       (do-hash-math body-forms 2)
       (do-hash-math keyword-atom 3)
       (do-hash-math 'defun-node 4))))

(defmethod calculate-node-counts ((obj defun-node))
 (with-slots (function-name parameters-list body-forms keyword-atom) obj
   (+ (calculate-node-counts function-name)
      (calculate-node-counts parameters-list)
      (calculate-node-counts body-forms)
      (calculate-node-counts keyword-atom)
      1)))

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

(defmethod calculate-hash ((obj function-call-node))
  (with-slots (func-lexem func-arg-forms) obj
    (+ (do-hash-math func-lexem 0)
       (do-hash-math func-arg-forms 1)
       (do-hash-math 'function-call-node 2))))

(defmethod calculate-node-counts ((obj function-call-node))
  (with-slots (func-lexem func-arg-forms) obj 
    (+ (calculate-node-counts func-lexem)
       (calculate-node-counts func-arg-forms)
       1)))

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

(defmethod calculate-hash ((obj parm-list-node))
  (with-slots (elements) obj
    (+ (calculate-hash elements)
       (do-hash-math 'parm-list-node 1))))

(defmethod calculate-hash ((obj bindings-list-node))
  (with-slots (elements) obj
    (+ (calculate-hash elements)
       (do-hash-math 'bindings-list-node 1))))

(defmethod calculate-hash ((obj q-list-node))
  (with-slots (elements) obj
    (+ (calculate-hash elements)
       (do-hash-math 'q-list-node 1))))

(defmethod calculate-node-counts ((obj list-node))
  (with-slots (elements) obj
    (+ 1
       (calculate-node-counts elements))))

(defmethod calculate-spec-length ((obj list))
  (1- (loop :for el :in obj
            :sum (1+ (calculate-spec-length el)))))

(defmethod calculate-hash ((obj list))
  (loop :for el :in obj
        :for i :from 0
        :sum (do-hash-math el i)))

(defmethod calculate-node-counts ((obj list))
  (loop :for el :in obj
        :for i :from 0
        :sum (calculate-node-counts el)))

(defmethod calculate-spec-length ((obj vector))
  (1- (loop :for el :across obj
            :sum (1+ (calculate-spec-length el)))))

(defmethod calculate-hash ((obj vector))
  (loop :for el :across obj
        :for i :from 0
        :sum (do-hash-math el i)))

(defmethod calculate-node-counts ((obj vector))
  (loop :for el :across obj
        :for i :from 0
        :sum (calculate-node-counts el)))

;;;QUOTE-NODE
(define-node quote-node ()
    ((quote-coord :accessor quote-coord
                  :initarg :quote-coord)
     (q-s-expr :accessor q-s-expr
               :initarg :q-s-expr)))

(defmethod calculate-spec-length ((obj quote-node))
  (with-slots (q-s-expr) obj
    (+ 1 (calculate-spec-length q-s-expr))))

(defmethod calculate-hash ((obj quote-node))
  (with-slots (q-s-expr) obj
    (+ (calculate-hash q-s-expr)
       (do-hash-math 'quote-node 1))))

(defmethod calculate-node-counts ((obj quote-node))
  (with-slots (q-s-expr) obj
    (+ 1
       (calculate-node-counts q-s-expr))))

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

(defmethod calculate-hash ((obj let-node))
  (with-slots (keyword-atom bindings body-forms)
      obj
    (+ (do-hash-math keyword-atom 0)
       (do-hash-math bindings 1)
       (do-hash-math body-forms 2)
       (do-hash-math 'let-node 3))))

(defmethod calculate-node-counts ((obj let-node))
  (with-slots (keyword-atom bindings body-forms)
      obj
    (+ 1
       (calculate-node-counts keyword-atom)
       (calculate-node-counts bindings)
       (calculate-node-counts body-forms))))

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

(defmethod calculate-hash ((obj let-binding-node))
  (with-slots (var-atom value-s-expr)
      obj
    (+ (do-hash-math var-atom 0)
       (do-hash-math value-s-expr 1)
       (do-hash-math 'let-binding-node 2))))

(defmethod calculate-node-counts ((obj let-binding-node))
  (with-slots (var-atom value-s-expr)
      obj
    (+ 1
       (calculate-node-counts var-atom)
       (calculate-node-counts value-s-expr))))

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

(defmethod calculate-hash ((obj defparameter-node))
  (with-slots (keyword-atom parameter-name value-s-expr)
      obj
    (+ (do-hash-math keyword-atom 0)
       (do-hash-math parameter-name 1)
       (do-hash-math value-s-expr 2)
       (do-hash-math 'defparameter-node 3))))

(defmethod calculate-node-counts ((obj defparameter-node))
  (with-slots (keyword-atom parameter-name value-s-expr)
      obj
    (+ 1
       (calculate-node-counts keyword-atom)
       (calculate-node-counts parameter-name)
       (calculate-node-counts value-s-expr))))

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

(defmethod calculate-hash ((obj if-node))
  (with-slots (keyword-atom test-s-expr then-s-expr else-s-expr)
      obj
    (+ (do-hash-math keyword-atom 0)
       (do-hash-math test-s-expr 1)
       (do-hash-math then-s-expr 2)
       (do-hash-math else-s-expr 3)
       (do-hash-math 'if-node 4))))

(defmethod calculate-node-counts ((obj if-node))
  (with-slots (keyword-atom test-s-expr then-s-expr else-s-expr)
      obj
    (+ 1
       (calculate-node-counts keyword-atom)
       (calculate-node-counts test-s-expr)
       (calculate-node-counts then-s-expr)
       (calculate-node-counts else-s-expr))))
