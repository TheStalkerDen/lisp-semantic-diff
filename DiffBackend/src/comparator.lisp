(uiop:define-package :diff-backend/comparator
    (:nicknames :comparator)
  (:use :cl :diff-backend/nodes
        :diff-backend/statistics
        :anaphora)
  (:export #:start-compare
           #:compare-results))

(in-package :diff-backend/comparator)

(declaim (optimize (debug 3)))

(defparameter *maybe-deleted-nodes* nil)

(defparameter *maybe-new-nodes* nil)

(defparameter *was-modified* nil)

(defparameter *cmp-memoization-table* nil)

(defun compare-results ()
  (let ((ver1-stats (get-stats 1))
        (ver2-stats (get-stats 2)))
    (loop :for stat-name :being :the :hash-keys :of ver1-stats
          :do
             (compare-specific-stats-hts
              (gethash stat-name ver1-stats)
              (gethash stat-name ver2-stats)))))

;;;it tests results after compare!!!
(defun compare-specific-stats-hts (ht1 ht2)
  (let* ((ht1-keys (alexandria:hash-table-keys ht1))
         (ht2-keys (alexandria:hash-table-keys ht2))
         (identical-keys (intersection ht1-keys ht2-keys :test #'string=))
         (unique-ht1-keys (set-difference ht1-keys identical-keys :test #'string=))
         (unique-ht2-keys (set-difference ht2-keys identical-keys :test #'string=)))
    (dolist (name identical-keys)
      (let ((val1 (gethash name ht1))
            (val2 (gethash name ht2)))
        (when (start-compare (first val1)
                             (first val2))
          (awhen (gethash name ht1)
            (setf (gethash name ht1)
                  (list (first it) :modified)))
          (awhen (gethash name ht2)
            (setf (gethash name ht2)
                  (list (first it) :modified))))))
    (dolist (name unique-ht1-keys)
      (awhen (gethash name ht1)
        (setf (gethash name ht1)
              (list (set-diff-status (first it) :deleted) :deleted))))
    (dolist (name unique-ht2-keys)
      (awhen (gethash name ht2)
        (setf (gethash name ht2)
              (list (set-diff-status (first it) :new) :new))))))

(defun start-compare (obj1 obj2)
  (let ((*maybe-deleted-nodes* nil
         ;(make-hash-table :test 'equal)
         )
        (*maybe-new-nodes* nil
                                        ;(make-hash-table :test 'equal)
        )
        (*was-modified* nil)
        (*cmp-memoization-table* (make-hash-table :test #'equal)))
    (compare obj1 obj2)
    (maybe-issue-resolver)
    *was-modified*))

(defun maybe-issue-resolver ()
  (dolist (maybe-deleted *maybe-deleted-nodes*)
    (dolist (maybe-new *maybe-new-nodes*)
      (acond
        ((gethash (cons (id maybe-deleted)
                        (id maybe-new))
                  *cmp-memoization-table*)
         (when (first it)
           (set-diff-status maybe-deleted `(:moved ,(id maybe-new)))
           (set-diff-status maybe-new `(:moved ,(id maybe-deleted)))
           (remove-from-memoiz-table :first-id (id maybe-deleted)
                                     :second-id (id maybe-new))
           (return)))
        ((compare maybe-deleted maybe-new)
         (set-diff-status maybe-deleted `(:moved ,(id maybe-new)))
         (set-diff-status maybe-new `(:moved ,(id maybe-deleted)))
         (return))))
    (setf *maybe-new-nodes*
          (remove-if
           (lambda (el)
             (when (listp (diff-status el))
               (eq (first (diff-status el)) :moved)))
           *maybe-new-nodes*)))
  (setf *maybe-deleted-nodes*
        (remove-if
         (lambda (el)
           (when (listp (diff-status el))
             (eq (first (diff-status el)) :moved)))
         *maybe-deleted-nodes*))
  (dolist (del-obj *maybe-deleted-nodes*)
    (set-diff-status del-obj :deleted))
  (dolist (new-obj *maybe-new-nodes*)
    (set-diff-status new-obj :new)))

(defgeneric compare (obj1 obj2)
  (:method (obj1 obj2)
    (warn "Unsupported compare for ~S and ~S ~%" obj1 obj2)
    (values nil 0 nil nil))
  (:documentation "Out1 - fully equal
Out2 - equal leaf-nodes
Out3 - diff-patch for obj1
Out4 - diff-patch for obj2"))

(defgeneric apply-diff-patch (obj diff-patch diff-status)
  (:method (obj diff-patch diff-status)
    (error "Unsupported apply-diff-patch for ~S" obj)))

(defun remove-from-memoiz-table (&key first-id second-id)
  (maphash (lambda (key val)
             (declare (ignore val))
             (when (and first-id
                        (= first-id (car key)))
               (remhash key *cmp-memoization-table*))
             (when (and second-id
                        (= second-id (cdr key)))
               (remhash key *cmp-memoization-table*)))
           *cmp-memoization-table*))

(defgeneric set-diff-status (obj status)
  (:method (obj1 obj2)
    (format t "set-diff-statud default method for object without diff-status-mixin. Something is wrong: ~A ~A~%" obj1 obj2)))

(defmethod set-diff-status ((obj diff-status-mixin) status)
  (setf (diff-status obj) status)
  (cond ((eq status :maybe-new)
         (push obj *maybe-new-nodes*))
        ((eq status :maybe-deleted)
         (push obj *maybe-deleted-nodes*)))
  (setf *was-modified* t))

(defmethod compare ((obj1 defun-node) (obj2 defun-node))
  (multiple-value-bind (is-equal eq-leaf-counts diff1 diff2)
      (compare
       (parameters-list obj1)
       (parameters-list obj2))
    (declare (ignore eq-leaf-counts))
    (unless is-equal
      (apply-diff-patch (parameters-list obj1) diff1 :maybe-deleted)
      (apply-diff-patch (parameters-list obj2) diff2 :maybe-new)))
  (multiple-value-bind (is-equal eq-leaf-counts diff1 diff2)
      (compare
       (body-forms obj1)
       (body-forms obj2))
    (declare (ignore eq-leaf-counts))
    (unless is-equal
      (apply-diff-patch (body-forms obj1) diff1 :maybe-deleted)
      (apply-diff-patch (body-forms obj2) diff2 :maybe-new)))
  nil)

(defmethod compare ((elems1 vector) (elems2 vector))
  (declare (optimize (debug 3)))
  (let ((is-equal t)
        (eq-leaf-counts 0)
        (diff-patch1 (make-hash-table))
        (diff-patch2 (make-hash-table))
        (first-non-equal-el-1)
        (first-non-equal-el-2)
        (last-non-equal-el-1 (when (> (length elems1)
                                      (length elems2))
                               (- (length elems1) (length elems2) 1)))
        (last-non-equal-el-2 (when (< (length elems1)
                                      (length elems2))
                               (- (length elems2) (length elems1) 1))))
                                        ;check if start elements are equal
    (loop :for el-index :from 0 :to (min (1- (length elems1))
                                         (1- (length elems2)))
          :do (multiple-value-bind (els-is-equal els-eq-leaf-counts)
                  (compare (aref elems1 el-index)
                           (aref elems2 el-index))
                (if els-is-equal
                    (incf eq-leaf-counts els-eq-leaf-counts)
                    (progn (setf first-non-equal-el-1 el-index
                                 first-non-equal-el-2 el-index)
                           (setf is-equal nil)
                           (return))))
          :finally
             (cond
               ((> (length elems1)
                   (length elems2))
                (setf first-non-equal-el-1 (length elems2))
                (setf last-non-equal-el-1 (1- (length elems1)))
                (setf is-equal nil))
               ((< (length elems1)
                   (length elems2))
                (setf first-non-equal-el-2 (length elems1))
                (setf last-non-equal-el-2 (1- (length elems2)))
                (setf is-equal nil))))
    (when (and first-non-equal-el-1 first-non-equal-el-2)
                                        ;check if end elements are equal
      (loop :for el1-index :from (1- (length elems1)) :downto first-non-equal-el-1
            :and el2-index :from (1- (length elems2)) :downto first-non-equal-el-2
            :do
               (multiple-value-bind (els-is-equal els-eq-leaf-counts)
                   (compare (aref elems1 el1-index)
                            (aref elems2 el2-index))
                 (if els-is-equal
                     (incf eq-leaf-counts els-eq-leaf-counts)
                     (progn
                       (setf last-non-equal-el-1 el1-index
                             last-non-equal-el-2 el2-index)
                       (setf is-equal nil)
                       (return))))))
    (cond
      ((and first-non-equal-el-1 last-non-equal-el-1
            (not (and first-non-equal-el-2 last-non-equal-el-2)))
       (loop :for el-index :from first-non-equal-el-1 :to last-non-equal-el-1
             :do
                (setf (gethash (id (aref elems1 el-index)) diff-patch1) t)))
      ((and first-non-equal-el-2 last-non-equal-el-2
            (not (and first-non-equal-el-1 last-non-equal-el-1)))
       (loop :for el-index :from first-non-equal-el-2 :to last-non-equal-el-2
             :do                
                (setf (gethash (id (aref elems2 el-index)) diff-patch2) t)))
      ((and first-non-equal-el-1 first-non-equal-el-2
            last-non-equal-el-1 last-non-equal-el-2)
       (let ((middle-els-1 (1+ (- last-non-equal-el-1 first-non-equal-el-1)))
             (middle-els-2 (1+ (- last-non-equal-el-2 first-non-equal-el-2))))
         (unless (or (<= middle-els-1 0)
                     (<= middle-els-2 0))
           (let ((lcs-array (make-array `(,(1+ middle-els-1)
                                          ,(1+ middle-els-2))
                                        :initial-element 0)))
             (loop
               :for i :from 1 :to middle-els-1
               :do
                  (loop
                    :for j :from 1 :to middle-els-2
                    :do (let ((el1 (aref elems1 (+ (1- i)
                                                   first-non-equal-el-1)))
                              (el2 (aref elems2 (+ (1- j)
                                                   first-non-equal-el-2))))
                          (multiple-value-bind (els-is-equal els-eq-leaf-counts diff1 diff2)
                              (compare el1
                                       el2)
                            (setf (gethash (cons (id el1)
                                                 (id el2))
                                           *cmp-memoization-table*)
                                  (list els-is-equal els-eq-leaf-counts diff1 diff2))
                            (if (> (or els-eq-leaf-counts 0) 0)
                                (setf (aref lcs-array i j)
                                      (+ els-eq-leaf-counts (aref lcs-array (1- i) (1- j))))
                                (setf (aref lcs-array i j)
                                      (max (aref lcs-array (1- i) j)
                                           (aref lcs-array i (1- j)))))))))
             (print lcs-array)
             (labels
                 ((%go-in-one-direction (i j direction)
                    (format t "i = ~S j = ~S ~S~%" i j direction)
                    (cond ((or (= j 0)
                               (= i 0))
                           nil)
                          ((eq direction :left)
                           (setf (gethash (id (aref elems2 (+ (1- j)
                                                              first-non-equal-el-2)))
                                          diff-patch2)
                                 t)
                           (%go-in-one-direction (1- i) j :left))
                          ((eq direction :up)
                           (setf (gethash (id (aref elems1 (+ (1- i)
                                                              first-non-equal-el-1)))
                                          diff-patch1)
                                 t)
                           (%go-in-one-direction i (1- j) :up))))
                  (%backtrack (i j)
                    (format t "i = ~S j = ~S~%" i j)
                    (cond
                      ((or (= i 0)
                           (= j 0))
                       nil)
                      ((and (= (aref lcs-array (1- i) j)
                               (aref lcs-array i (1- j)))
                            (/= (aref lcs-array (1- i) j)
                                (aref lcs-array i j)))
                       (let* ((el1 (aref elems1 (+ (1- i)
                                                   first-non-equal-el-1)))
                              (el2 (aref elems2 (+ (1- j)
                                                   first-non-equal-el-2)))
                              (cmp-info (gethash (cons (id el1)
                                                       (id el2))
                                                 *cmp-memoization-table*)))
                         (unless (first cmp-info)
                           (setf (gethash (id el1) diff-patch1) (third cmp-info))
                           (setf (gethash (id el2) diff-patch2) (fourth cmp-info)))
                         (remove-from-memoiz-table :first-id (id el1)
                                                   :second-id (id el2)))
                       (cond
                         ((= (1- j) 0)
                          (%go-in-one-direction (1- i) j :up))
                         ((= (1- i) 0)
                          (%go-in-one-direction i (1- j) :left))
                         (t
                          (%backtrack (1- i) (1- j)))))
                      ((>= (aref lcs-array (1- i) j)
                           (aref lcs-array i (1- j)))
                       (setf (gethash (id (aref elems1 (+ (1- i)
                                                          first-non-equal-el-1)))
                                      diff-patch1)
                             t)
                       (when (= (1- i) 0)
                         (setf (gethash (id (aref elems2 (+ (1- j)
                                                            first-non-equal-el-2)))
                                        diff-patch2)
                               t))
                       (if (and (= (1- i) 0) (not (= (1- j) 0)))
                           (%go-in-one-direction i (1- j) :left)
                           (%backtrack (1- i) j)))
                      ((>= (aref lcs-array i (1- j))
                           (aref lcs-array (1- i) j))
                       (setf (gethash (id (aref elems2 (+ (1- j)
                                                          first-non-equal-el-2)))
                                      diff-patch2)
                             t)
                       (when (= (1- j) 0)
                         (setf (gethash (id (aref elems1 (+ (1- i)
                                                            first-non-equal-el-1)))
                                        diff-patch1)
                               t))
                       (if (and (= (1- j) 0) (not (= (1- i) 0)))
                           (%go-in-one-direction (1- i) j :up)
                           (%backtrack i (1- j)))))))
               (incf eq-leaf-counts (aref lcs-array middle-els-1 middle-els-2))
               (%backtrack middle-els-1 middle-els-2)))))))
    (values is-equal eq-leaf-counts diff-patch1 diff-patch2)))

(defmethod apply-diff-patch ((obj vector) diff-patch diff-status)
  (loop :for el :across obj
        :do
           (awhen (gethash (id el) diff-patch)
             (if (eq t it)
                 (set-diff-status el diff-status)
                 (apply-diff-patch el it diff-status)))))

(defmethod compare ((obj1 function-call-node) (obj2 function-call-node))
  (let ((equal-leaf-nodes-count 0)
        (is-equal t)
        (diff-patch1)
        (diff-patch2))
    (if (compare (func-lexem obj1)
                 (func-lexem obj2))
        (incf equal-leaf-nodes-count)
        (progn
          (setf is-equal nil)
          (setf diff-patch1
                (acons :func-lexem t diff-patch1)
                diff-patch2
                (acons :func-lexem t diff-patch2))))
    (multiple-value-bind (is= leaf-counts diff1 diff2)
        (compare (func-arg-forms obj1)
                 (func-arg-forms obj2))
      (incf equal-leaf-nodes-count leaf-counts)
      (unless is=
        (setf diff-patch1
              (acons :func-arg-forms
                     (if (> leaf-counts 0)
                         diff1
                         t)
                     diff-patch1)
              diff-patch2
              (acons :func-arg-forms
                     (if (> leaf-counts 0)
                         diff2
                         t)
                     diff-patch2)))
      (setf is-equal (and is-equal is=)))
    (values is-equal equal-leaf-nodes-count diff-patch1 diff-patch2)))

(defmethod apply-diff-patch ((obj function-call-node) diff-patch diff-status)
  (when (assoc :func-lexem diff-patch)
    (set-diff-status (func-lexem obj) diff-status))
  (awhen (assoc :func-arg-forms diff-patch)
    (if (eq t (rest it))
        (set-diff-status (func-arg-forms obj) diff-status)
        (apply-diff-patch (func-arg-forms obj) (rest it) diff-status))))

(defmethod compare ((obj1 list-node) (obj2 list-node))
  (if (or (elements obj1)
          (elements obj2))
      (compare (elements obj1)
               (elements obj2))
      (values t 1)))

(defmethod apply-diff-patch ((obj list-node) diff-patch diff-status)
  (apply-diff-patch (elements obj) diff-patch diff-status))

(defmethod compare ((obj1 lexem-wrapper-node) (obj2 lexem-wrapper-node))
  (let ((lex-info1 (lexem-info obj1))
        (lex-info2 (lexem-info obj2)))
    (unless (eq (lexer:lexem-type lex-info1) (lexer:lexem-type lex-info2))
      (return-from compare (values nil 0 (id obj1) (id obj2))))
    (cond
      ((eq (lexer:lexem-type lex-info1) :symbol)
       (if (string= (string-upcase (lexer:lexem-string lex-info1))
                    (string-upcase (lexer:lexem-string lex-info2)))
           (return-from compare (values t 1))))
      (t
       (if (string= (lexer:lexem-string lex-info1)
                    (lexer:lexem-string lex-info2))
           (return-from compare (values t 1))))))
  (return-from compare (values nil 0 (id obj1) (id obj2))))
