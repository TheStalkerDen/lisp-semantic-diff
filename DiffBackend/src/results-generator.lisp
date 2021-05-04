(uiop:define-package :diff-backend/results-generator
    (:nicknames :res-gen)
  (:use :cl :cl-json
        :diff-backend/nodes
        :diff-backend/lexer
        :diff-backend/statistics)
  (:export #:get-json-res
           #:get-stats-res
           #:get-json-comments
           #:get-lexer-errors-msgs-json
           #:get-lexems-json))

(in-package :diff-backend/results-generator)

(defmacro add-to-ht (key value)
  `(setf (gethash ,key ht) ,value))

(defmacro with-ht (&body body)
  `(let ((ht (make-hash-table :test #'equal)))
     (progn
       ,@body)
     ht))

(defun get-json-res (obj stream)
  (encode-json (gener-res-object obj) stream))

(defun get-stats-res (stream)
  (encode-json
   (alexandria:alist-hash-table
    `(("old-ver" . ,(gen-stats-ht-for-json 1))
      ("new-ver" . ,(gen-stats-ht-for-json 2))))
   stream))

(defun get-json-comments (comment-table stream)
  (encode-json
   (with-ht
     (loop :for line-num :being :the :hash-key :in comment-table :using (:hash-value val)
           :do (setf (gethash line-num ht) (alexandria:plist-hash-table val))))
   stream))

(defun get-lexer-errors-msgs-json (lexer-errors-msgs stream)
  (encode-json
   lexer-errors-msgs
   stream))

(defun get-lexems-json (lexems stream)
  (encode-json
   lexems
   stream))

(defgeneric gener-res-object (obj))

(defmethod gener-res-object ((obj defun-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "props" (alexandria:alist-hash-table
                        `((:is-defun . ,(get-lexem-name (function-name obj))))))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (list*
                        (gener-res-object (keyword-lexem obj))
                        (gener-res-object (function-name obj))
                        (gener-res-object (parameters-list obj))
                        (gener-res-object (body-forms obj))))))

(defmethod gener-res-object ((obj vector))
  (map 'list #'gener-res-object obj))

(defmethod gener-res-object ((obj list))
  (map 'list #'gener-res-object obj))

(defmethod gener-res-object ((obj function-call-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (list*
                        (gener-res-object (func-lexem obj))
                        (gener-res-object (func-arg-forms obj))))))

(defmethod gener-res-object ((obj list-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
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

