(uiop:define-package :diff-backend/experiments
    (:use :cl :diff-backend/comparator
          :diff-backend/results-generator)
  (:import-from :alexandria
   :read-file-into-string)
  (:import-from :diff-backend/lexer
   :lexer)
  (:import-from :diff-backend/parser
   :parser)
  (:import-from :diff-backend/abstract-sem-tree-generator
   :abstract-sem-tree-gen)
  (:import-from :diff-backend/statistics
                :init-stats))
(in-package :diff-backend/experiments)

(defparameter *dotimes* 2)

(defun expers-with-equal-files ()
  (compare-two-files-in "equal"))

(defun expers-with-full-not-equal-files ()
  (compare-two-files-in "not-equal"))

(defun expers-with-modified-at-start-files ()
  (compare-two-files-in "st-mod"))
    

(defun compare-two-files-in (folder)
  (format t "Current method is ~S ~%" *current-method*)
  (loop
    :for i :from 1 :to 9
    :do
       (progn
         (format t "-------------------------------------------~%")
         (format t "i = ~d ~%" i)
         (let ((pattern "../../lisp-files/~A/f~Av~A.lisp"))
           (compare-two-files (format nil pattern folder i 1) 
                              (format nil pattern folder i 2))))))


(defun compare-two-files (filepath1 filepath2)
  (let ((str1 (read-file-into-string filepath1))
        (str2 (read-file-into-string filepath2)))
    (init-stats)
    (compare-two-str str1 str2)))

(defun compare-two-str (str1 str2)
  (let ((ast-1 (get-abstract-sem-tree-from-string str1 1))
        (ast-2 (get-abstract-sem-tree-from-string str2 2))
        (start-time (get-internal-run-time)))
    (dotimes (i *dotimes*)
      (start-asts-comparing ast-1 ast-2))
    (log:info "Exec time: ~f sec" (/ (- (get-internal-run-time) start-time) (* *dotimes* internal-time-units-per-second)))))

(defun get-abstract-sem-tree-from-string (str cur-file)
  (abstract-sem-tree-gen (parser (lexer str)) :curr-file cur-file))
