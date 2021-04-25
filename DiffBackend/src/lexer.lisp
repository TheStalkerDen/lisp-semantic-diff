(uiop:define-package :diff-backend/lexer
    (:nicknames :lexer)
  (:use :cl :diff-backend/utils)
  (:export #:lexer
           #:is-lexem-symbol?=
           #:make-lexem
           #:equal-lexem?))

(in-package :diff-backend/lexer)

(declaim (optimize safety))

(defclass* lexem ()
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

(defun equal-lexem? (lex1 lex2)
  (with-slots ((line1 line)
               (column1 column)
               (type1 type)
               (string1 string))
      lex1
    (with-slots ((line2 line)
                 (column2 column)
                 (type2 type)
                 (string2 string))
        lex2
      (and (= line1 line2)
           (= column1 column2)
           (eq type1 type2)
           (string= string1 string2)))))

(defun make-lexem (string line column type)
  (make-instance 'lexem
                 :string string
                 :line line
                 :column column
                 :type type))

(defun is-lexem-symbol?= (lexem symbol-string)
  (when (eq (lexem-type lexem) :symbol)
    (string-equal (string-upcase (lexem-string lexem))
                  (string-upcase symbol-string))))

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defvar *special-symbols* '(#\+ #\- #\* #\? #\! #\$
                            #\^ #\& #\@ #\% #\< #\> #\~ #\. #\= #\_))

(defun is-whitespace? (char)
  (when char
    (member char *whitespaces*)))

(defun is-special-symbol? (char)
  (when char
    (member char *special-symbols*)))

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
        (comments-table (make-hash-table))
        (lex-errors)
        (line 1)
        (cur-lexem-column 1)
        (cur-lexem-line 1)
        (column 1)
        (lexem-l))
    (prog
        ((cur-char (read-char stream nil)))
     COMMON
       (setf cur-lexem-column column)
       (setf cur-lexem-line line)
       (cond
         ((digit? cur-char) (go INTEGER))
         ((or (alpha? cur-char)
              (is-special-symbol? cur-char))
          (go SYMBOL))
         ((ch= cur-char #\( ) (go OUT_LEFT_PARENT))
         ((ch= cur-char #\)) (go OUT_RIGHT_PARENT))
         ((ch= cur-char #\') (go QUOTE))
         ((ch= cur-char #\") (go STRING))
         ((ch= cur-char #\;) (go COMMENT))
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
         ((or (alpha? cur-char)
              (is-special-symbol? cur-char))
          (go SYMBOL))
         (t (go OUT_INTEGER)))
     OUT_INTEGER
       (push (make-lexem (coerce (reverse lexem-l) 'string)
                         cur-lexem-line
                         cur-lexem-column
                         :integer)
             lexems)
       (setf lexem-l nil)
       (go COMMON)
     SYMBOL
       (incf column)
       (push cur-char lexem-l)
       (setf cur-char (read-char stream nil))
       (cond ((or (digit-alpha? cur-char)
                  (is-special-symbol? cur-char))
              (go SYMBOL))
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
     QUOTE
       (push (make-lexem "'"
                         line
                         cur-lexem-column
                         :quote)
             lexems)
       (incf column)
       (setf cur-char (read-char stream nil))
       (go COMMON)
     STRING
       (when (ch= cur-char #\Newline)
         (setf column 0)
         (incf line))
       (incf column)
       (push cur-char lexem-l)
       (setf cur-char (read-char stream nil))
       (cond ((ch= cur-char #\\)
              (go STRING_ESCAPE_SYMBOL))
             ((ch= cur-char #\")
              (push cur-char lexem-l)
              (go OUT_STRING))
             (t (go STRING)))
     STRING_ESCAPE_SYMBOL
       (incf column)
       (push cur-char lexem-l)
       (setf cur-char (read-char stream nil))
       (go STRING)
     OUT_STRING
       (push (make-lexem (coerce (reverse lexem-l) 'string)
                         cur-lexem-line
                         cur-lexem-column
                         :string)
             lexems)
       (setf lexem-l nil)
       (setf cur-char (read-char stream nil))
       (incf column)
       (go COMMON)
     COMMENT
       (incf column)
       (push cur-char lexem-l)
       (setf cur-char (read-char stream nil))
       (cond ((or (ch= cur-char #\Newline)
                  (ch= cur-char #\Return)
                  (null cur-char))
              (go COMMENT_OUT))
             (t (go COMMENT)))
     COMMENT_OUT
       (setf (gethash cur-lexem-line comments-table)
             `(:comment ,(coerce (reverse lexem-l) 'string)
               :column ,cur-lexem-column))
       (setf lexem-l nil)
       (go COMMON)
     END
       (return))
    (values (reverse lexems)
            comments-table
            (reverse lex-errors))))
