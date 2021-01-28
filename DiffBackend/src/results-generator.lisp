(uiop:define-package :diff-backend/results-generator
    (:nicknames :res-gen)
  (:use :cl :cl-json
        :diff-backend/nodes
        :diff-backend/lexer)
  (:export #:get-json-res))

(in-package :diff-backend/results-generator)

(defun get-json-res (obj stream)
  (encode-json (gener-res-object obj) stream))

(defgeneric gener-res-object (obj))

(defmacro add-to-ht (key value)
  `(setf (gethash ,key ht) ,value))

(defmacro with-ht (&body body)
  `(let ((ht (make-hash-table :test #'equal)))
     (progn
       ,@body)
     ht))

(defmethod gener-res-object ((obj defun-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (encode-json-alist-to-string
                           (parenthesis-info obj)))
    (add-to-ht "elems" (list*
                        (gener-res-object (keyword-lexem obj))
                        (gener-res-object (function-name obj))
                        (gener-res-object (parameters-list obj))
                        (gener-res-object (body-forms obj))))))

(defmethod gener-res-object ((obj list))
  (mapcar (lambda (el)
            (gener-res-object el))
          obj))

(defmethod gener-res-object ((obj function-call-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (encode-json-alist-to-string
                           (parenthesis-info obj)))
    (add-to-ht "elems" (list*
                        (gener-res-object (func-lexem obj))
                        (gener-res-object (func-arg-forms obj))))))

(defmethod gener-res-object ((obj list-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (encode-json-alist-to-string
                           (parenthesis-info obj)))
    (add-to-ht "elems" (gener-res-object (elements obj)))))

(defmethod gener-res-object ((obj lexem-wrapper-node))
  (with-ht
    (add-to-ht "type" "lexem")
    (add-to-ht "diff-st" (diff-status obj))
    (let ((lex (lexem-info obj)))
      (add-to-ht "lexem-coord" `(,(lexem-line lex)
                                  ,(lexem-column lex)))
      (add-to-ht "lex-type" (lexem-type lex))
      (add-to-ht "string" (lexem-string lex)))))

(defmethod gener-res-object ((obj lexem))
  (with-ht
    (add-to-ht "type" "lexem")
    (add-to-ht "diff-st" "same")
    (add-to-ht "lexem-coord" `(,(lexem-line obj)
                                ,(lexem-column obj)))
    (add-to-ht "lex-type" (lexem-type obj))
    (add-to-ht "string" (lexem-string obj))))