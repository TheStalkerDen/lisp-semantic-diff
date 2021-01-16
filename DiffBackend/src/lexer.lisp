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

(defun make-lexem (string line column type)
  (make-instance 'lexem
                 :string string
                 :line line
                 :column column
                 :type type))

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defun is-whitespace? (char)
  (when char
    (member char *whitespaces*)))

(defun digit? (char)
  (when char
    (digit-char-p char)))

(defun alpha? (char)
  (when char
    (alpha-char-p char)))

(defun digit-alpha? (char)
  (when char
    (alphanumericp char)))

(defun ch= (char1 char2)
  (when char1
    (char= char1 char2)))

(defun lexer (file-str)
  (let ((stream (make-string-input-stream file-str))
        (lexems)
        (line 1)
        (cur-lexem-column 1)
        (column 1)
        (lexem-l))
    (prog
        ((cur-char (read-char stream nil)))
     COMMON
     (setf cur-lexem-column column)
     (cond
       ((digit? cur-char) (go INTEGER))
       ((alpha? cur-char) (go SYMBOL))
       ((ch= cur-char #\( ) (go OUT_LEFT_PARENT))
       ((ch= cur-char #\) ) (go OUT_RIGHT_PARENT))
       ((is-whitespace? cur-char) (go WHITESPACE))
       ((null cur-char) (go END))
       (t (error "Incorrect char ~s~%" cur-char)))
     INTEGER
     (incf column)
     (push cur-char lexem-l)
     (setf cur-char (read-char stream nil))
     (cond
       ((digit? cur-char)
        (go INTEGER))
       ((alpha? cur-char)
        (go SYMBOL))
       (t (go OUT_INTEGER)))
     OUT_INTEGER
     (push (make-lexem (coerce (reverse lexem-l) 'string)
                       line
                       cur-lexem-column
                       :integer)
           lexems)
     (setf lexem-l nil)
     (go COMMON)
     SYMBOL
     (incf column)
     (push cur-char lexem-l)
     (setf cur-char (read-char stream nil))
     (cond ((digit-alpha? cur-char) (go SYMBOL))
           (t (go OUT_SYMBOL)))
     OUT_SYMBOL
     (push (make-lexem (coerce (reverse lexem-l) 'string)
                       line
                       cur-lexem-column
                       :symbol)
           lexems)
     (setf lexem-l nil)
     (go COMMON)
     OUT_LEFT_PARENT
     (push (make-lexem "("
                       line
                       cur-lexem-column
                       :left-parent)
           lexems)
     (setf cur-char (read-char stream nil))
     (incf column)
     (go COMMON)
     OUT_RIGHT_PARENT
     (push (make-lexem ")"
                       line
                       cur-lexem-column
                       :right-parent)
           lexems)
     (setf cur-char (read-char stream nil))
     (incf column)
     (go COMMON)
     WHITESPACE
     (when (ch= cur-char #\Newline)
       (setf column 0)
       (incf line))
     (incf column)
     (setf cur-char (read-char stream nil))
     (cond ((is-whitespace? cur-char) (go WHITESPACE))
           (t (go COMMON)))
     END
     (return (reverse lexems)))))
