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
           #:get-parser-error-msg-json
           #:get-semantic-errors-msgs-json
           #:get-lexems-json
           #:get-moved-s-exprs-json))

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

(defun get-parser-error-msg-json (parser-error-msg stream)
  (encode-json
   parser-error-msg
   stream))

(defun get-semantic-errors-msgs-json (semantic-errors-msgs stream)
  (encode-json
   semantic-errors-msgs
   stream))

(defun get-lexems-json (lexems stream)
  (encode-json
   lexems
   stream))

(defun get-moved-s-exprs-json (moved-s-exprs-list stream)
  (encode-json
   moved-s-exprs-list
   stream))

(defgeneric gener-res-object (obj))

(defmethod gener-res-object ((obj illegal-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (when (top? obj)
      (add-to-ht "isIllegalNode" t))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (gener-res-object (elements obj)))))

(defmethod gener-res-object ((obj defun-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "props" (alexandria:alist-hash-table
                        `((:isTopLevel . ,(string-upcase (get-lexem-name (function-name obj)))))))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (list*
                        (gener-res-object (keyword-atom obj))
                        (gener-res-object (function-name obj))
                        (gener-res-object (parameters-list obj))
                        (gener-res-object (body-forms obj))))))

(defmethod gener-res-object ((obj defparameter-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "props" (alexandria:alist-hash-table
                        `((:isTopLevel . ,(string-upcase (get-lexem-name (parameter-name obj)))))))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (list
                        (gener-res-object (keyword-atom obj))
                        (gener-res-object (parameter-name obj))
                        (gener-res-object (value-s-expr obj))))))

(defmethod gener-res-object ((obj let-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (list*
                        (gener-res-object (keyword-atom obj))
                        (gener-res-object (bindings obj))
                        (gener-res-object (body-forms obj))))))

(defmethod gener-res-object ((obj let-binding-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (remove nil
                               (list
                                (gener-res-object (var-atom obj))
                                (when (value-s-expr obj)
                                  (gener-res-object (value-s-expr obj))))))))

(defmethod gener-res-object ((obj vector))
  (map 'list #'gener-res-object obj))

(defmethod gener-res-object ((obj list))
  (map 'list #'gener-res-object obj))

(defmethod gener-res-object ((obj function-call-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (list*
                        (gener-res-object (func-lexem obj))
                        (gener-res-object (func-arg-forms obj))))))

(defmethod gener-res-object ((obj list-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems" (gener-res-object (elements obj)))))

(defmethod gener-res-object ((obj atom-node))
  (with-ht
    (add-to-ht "type" "atom")
    (add-to-ht "id" (id obj))
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

(defmethod gener-res-object ((obj quote-node))
  (with-ht
      (add-to-ht "type" "quote")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "quote-coord" (quote-coord obj))
    (add-to-ht "q-s-expr" (gener-res-object (q-s-expr obj)))))

(defmethod gener-res-object ((obj if-node))
  (with-ht
    (add-to-ht "type" "list")
    (add-to-ht "id" (id obj))
    (add-to-ht "diff-st" (diff-status obj))
    (add-to-ht "par-info" (alexandria:alist-hash-table
                           (parenthesis-info obj) :test #'equal))
    (add-to-ht "elems"
               (remove nil (list
                            (gener-res-object (keyword-atom obj))
                            (gener-res-object (test-s-expr obj))
                            (gener-res-object (then-s-expr obj))
                            (when (else-s-expr obj)
                              (gener-res-object (else-s-expr obj))))))))
