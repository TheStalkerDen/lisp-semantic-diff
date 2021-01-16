(uiop:define-package :diff-backend/lexer
    (:nicknames :lexer)
  (:use :cl)
  (:export #:lexer
           #:lexem))

(in-package :diff-backend/lexer)

(declaim (optimize safety))

(defclass lexem ()
  ((line
    :accessor lexem-line
    :initarg :line
    :type integer)
   (column
    :accessor lexem-column
    :initarg :column
    :type integer)
   (type
    :accessor lexem-type
    :initarg :type
    :type symbol)
   (string
    :accessor lexem-string
    :initarg :string
    :type string))
  (:documentation "Lexem info"))

(defmethod print-object ((lex lexem) stream)
  (with-slots (line column string) lex
    (print-unreadable-object (lex stream)
      (format stream "Lexem: ~S (~d:~d)" string line column))))

(defun lexer (file-str)
  nil)
