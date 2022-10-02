(uiop:define-package :diff-backend/comparator-new
    (:nicknames :comparator-new)
  (:use :cl :diff-backend/nodes
        :diff-backend/statistics
   :anaphora)
  (:import-from :alexandria
                #:mappend)
  (:export #:start-new-version-of-asts-comparing))

(in-package :diff-backend/comparator-new)

(declaim (optimize (debug 3)))

(defparameter *maybe-deleted-nodes* nil)

(defparameter *maybe-new-nodes* nil)

(defparameter *was-modified* nil)

(defparameter *cmp-memoization-table* nil)

(defclass moved-s-expr-info ()
  ((s-expr-id1 :accessor s-expr-id1
               :initarg :s-expr-id1)
   (s-expr-id2 :accessor s-expr-id2
               :initarg :s-expr-id2)
   (start-coord-of-id1 :accessor start-coord-of-id1
                       :initarg :start-coord-of-id1)
   (start-coord-of-id2 :accessor start-coord-of-id2
                       :initarg :start-coord-of-id2)
   (end-coord-of-id1 :accessor end-coord-of-id1
                     :initarg :end-coord-of-id1)
   (end-coord-of-id2 :accessor end-coord-of-id2
                     :initarg :end-coord-of-id2)))

(defparameter *moved-s-exprs-list* nil)

(defun start-new-version-of-asts-comparing (ast-1 ast-2)
  (log:trace "new version of asts comparing starting")
  (let ((def-s-exprs-stats-1 (get-stats 1))
        (def-s-exprs-stats-2 (get-stats 2))
        (*moved-s-exprs-list*))
    (loop
      :for def-s-expr-type :being :the :hash-keys :of def-s-exprs-stats-1
      :do (compare-def-s-nodes def-s-expr-type
                               def-s-exprs-stats-1
                               def-s-exprs-stats-2))
    (values ast-1 ast-2 (reverse *moved-s-exprs-list*))))

(defun compare-def-s-nodes (def-s-expr-type def-s-exprs-stats-1 def-s-exprs-stats-2)
  (let ((ht1 (aif (gethash def-s-expr-type def-s-exprs-stats-1)
                  it
                  (make-hash-table :test #'equal)))
        (ht2 (aif (gethash def-s-expr-type def-s-exprs-stats-2)
                  it
                  (make-hash-table :test #'equal))))
    (multiple-value-bind (ident-ids unique-ids1 unique-ids2)
        (get-identical-id-names ht1 ht2)
      (dolist (id unique-ids1)
        (awhen (gethash id ht1)
          (setf (gethash id ht1)
                (list (set-diff-status (first it) :deleted) :deleted))))
      (dolist (id unique-ids2)
        (awhen (gethash id ht2)
          (setf (gethash id ht2)
                (list (set-diff-status (first it) :new) :new))))
      (dolist (id ident-ids)
        (let ((val1 (gethash id ht1))
              (val2 (gethash id ht2)))
          (when (compare-def-nodes (first val1)
                                   (first val2))
            (awhen (gethash id ht1)
              (setf (gethash id ht1)
                    (list (first it) :modified)))
            (awhen (gethash id ht2)
              (setf (gethash id ht2)
                    (list (first it) :modified)))))))))

(defun get-identical-id-names (ht1 ht2)
  (let* ((ht1-keys (alexandria:hash-table-keys ht1))
         (ht2-keys (alexandria:hash-table-keys ht2))
         (identical-keys (intersection ht1-keys ht2-keys :test #'string=))
         (unique-ht1-keys (set-difference ht1-keys identical-keys :test #'string=))
         (unique-ht2-keys (set-difference ht2-keys identical-keys :test #'string=)))
    (values identical-keys unique-ht1-keys unique-ht2-keys)))

(defun deep-equal (obj1 obj2)
  (if (or (and (integerp obj1)
               (integerp obj2))
          (and (stringp obj1)
               (stringp obj2))
          (equal (type-of obj1) (type-of obj2)))
      (if (typecase obj1 
            (standard-object
             (loop :for slot :in (closer-mop:class-slots (class-of obj1))
                   :for slot-name = (closer-mop:slot-definition-name slot)
                   :always (or (and (null (slot-boundp obj1 slot-name))
                                    (null (slot-boundp obj2 slot-name)))
                               (eq slot-name 'id)
                               (if (deep-equal (slot-value obj1 slot-name)
                                               (slot-value obj2 slot-name))
                                   t
                                   (progn (log:debug "~A" obj1)
                                          (log:debug "~A" obj2)
                                          nil)))))
            (list  (if (= (length obj1) (length obj2))
                       (every #'deep-equal obj1 obj2)
                       (progn
                         (log:debug "Length not equal!~%")
                         (log:debug "Length ~A : ~A~%" (length obj1) obj1)
                         (log:debug "Length ~A : ~A~%" (length obj2) obj2)
                         nil)))
            (vector (if (= (length obj1) (length obj2))
                       (every #'deep-equal obj1 obj2)
                       (progn
                         (log:debug "Length not equal!~%")
                         (log:debug "Length ~A : ~A~%" (length obj1) obj1)
                         (log:debug "Length ~A : ~A~%" (length obj2) obj2)
                         nil)))
            (t (unless (equalp obj1 obj2)
                 (log:debug "~A" obj1)
                 (log:debug "~A" obj2)
                 (return-from deep-equal nil))
             t))
          t
          (progn
            (log:debug "~A" obj1)
            (log:debug "~A" obj2)
            nil))
      (progn (log:debug "Not equal types")
             (log:debug "Type: ~A obj1: ~A ~%" (type-of obj1) obj1)
             (log:debug "Type: ~A obj2: ~A ~%" (type-of obj2) obj2)
             nil)))

(defun compare-def-nodes (obj1 obj2)
  (let ((*maybe-deleted-nodes* nil)
        (*maybe-new-nodes* nil)
        (*was-modified* nil)
        (*cmp-memoization-table* (make-hash-table :test #'equal)))
    (unless (and (= (hash obj1)
                    (hash obj2))
                 (= (node-counts obj1)
                    (node-counts obj2))
                 (deep-equal obj1 obj2))
      (compare obj1 obj2)
      (maybe-issue-resolver))
    *was-modified*))

(defparameter *maybe-match-list* nil)

(defun add-to-maybe-match-list (obj1 obj2 nodes-prefix)
  (log:debug t "~%Add to *maybe-match-list*~%")
  (log:debug t "obj1 = ~S~%" obj1)
  (log:debug t "obj2 = ~S~%" obj2)
  (log:debug t "nodes-prefix = ~S~%" nodes-prefix)
  (set-diff-status obj1 :maybe-match :no-push t)
  (set-diff-status obj2 :maybe-match :no-push t)
  (push (list obj1 obj2 nodes-prefix)
        *maybe-match-list*))

(defun add-to-moved-s-exprs-list (node1 node2)
  (log:debug t "~%Add to moved-s-exprs-list~%")
  (let ((first-and-last-coord1 (get-first-and-last-coord node1))
        (first-and-last-coord2 (get-first-and-last-coord node2)))
    (push (make-instance
           'moved-s-expr-info
           :s-expr-id1 (id node1)
           :s-expr-id2 (id node2)
           :start-coord-of-id1 (first first-and-last-coord1)
           :end-coord-of-id1 (second first-and-last-coord1)
           :start-coord-of-id2 (first first-and-last-coord2)
           :end-coord-of-id2 (second first-and-last-coord2))
          *moved-s-exprs-list*)) )

(defun maybe-issue-resolver ()
  (let (*maybe-match-list*)
    (log:debug "Initially *maybe-deleted-nodes* = ~S~%" *maybe-deleted-nodes*)
    (log:debug "Initially *maybe-new-nodes* = ~S~%~%" *maybe-new-nodes*)
    (loop
      :while (and *maybe-deleted-nodes*
                  *maybe-new-nodes*)
      :do
         (dolist (maybe-deleted *maybe-deleted-nodes*)
           (dolist (maybe-new *maybe-new-nodes*)
             (acond
              ((first (gethash (cons (id maybe-deleted)
                                     (id maybe-new))
                               *cmp-memoization-table*))
               (log:debug "~%memoiz-work~%")
               (set-diff-status maybe-deleted `(:moved ,(id maybe-new)))
               (set-diff-status maybe-new `(:moved ,(id maybe-deleted)))
               (remove-from-memoiz-table :first-id (id maybe-deleted)
                                         :second-id (id maybe-new))
               (add-to-moved-s-exprs-list maybe-deleted maybe-new)
               (return))
              ((compare maybe-deleted maybe-new)
               (log:debug "~%was successfully compare~%")
               (set-diff-status maybe-deleted `(:moved ,(id maybe-new)))
               (set-diff-status maybe-new `(:moved ,(id maybe-deleted)))
               (add-to-moved-s-exprs-list maybe-deleted maybe-new)
               (return))
              (t
               (log:debug "~%try traverse-and-compare~%")
               (when (traverse-and-compare
                      maybe-deleted
                      maybe-new
                      (list (id maybe-new)))
                 (return)))))
           (setf *maybe-new-nodes*
                 (remove-if
                  (lambda (el)
                    (when (listp (diff-status el))
                      (eq (first (diff-status el)) :moved)))
                  *maybe-new-nodes*))
           (log:debug "~%New *maybe-new-nodes* = ~S~%~%" *maybe-new-nodes*))
         (setf *maybe-deleted-nodes*
               (remove-if
                (lambda (el)
                  (or (eq (diff-status el) :maybe-match)
                      (when (listp (diff-status el))
                        (eq (first (diff-status el)) :moved))))
                *maybe-deleted-nodes*))
         (let ((maybe-deleted-nodes *maybe-deleted-nodes*))
           (setf *maybe-deleted-nodes*
                 (remove
                  nil
                  (mappend
                   #'colapse-node
                   (mapc
                    (lambda (node)
                      (when (eq (diff-status node) :maybe-deleted)
                        (set-diff-status node :deleted)))
                    maybe-deleted-nodes))))
           (log:debug "~%New *maybe-deleted-nodes* = ~S~%~%" *maybe-deleted-nodes*)))
    (log:debug "~%*maybe-match-list* = ~S~%~%" *maybe-match-list*)
    (let ((maybe-match-list *maybe-match-list*))
      (dolist (el maybe-match-list)
        (let ((obj1 (first el))
              (obj2 (second el)))
          (set-diff-status obj1 `(:moved ,(id obj2)))
          (set-diff-status obj2 `(:moved ,(id obj1)))
          (add-to-moved-s-exprs-list obj1 obj2)))) 
    (dolist (del-obj *maybe-deleted-nodes*)
      (set-diff-status del-obj :deleted))
    (dolist (new-obj *maybe-new-nodes*)
      (set-diff-status new-obj :new))))


(defgeneric compare (obj1 obj2)
  (:method (obj1 obj2)
    (values nil 0 (when obj1 (id obj1)) (when obj2 (id obj2))))
  (:documentation "Out1 - fully equal
Out2 - equal leaf-nodes
Out3 - diff-patch for obj1
Out4 - diff-patch for obj2"))

(defgeneric apply-diff-patch (obj diff-patch diff-status)
  (:method (obj diff-patch diff-status)
    (error "Unsupported apply-diff-patch for ~S" obj)))

(defgeneric get-first-and-last-coord (obj)
  (:method (obj)
    (warn "No support for get-first-and-last-coord")
    `((0 0) (0 0))))

(defmethod get-first-and-last-coord ((obj parenthesis-mixin))
  (let ((par-info (parenthesis-info obj)))
    `(,(rest (first par-info))
      ,(rest (second par-info)))))

(defmethod get-first-and-last-coord ((obj atom-node))
  (let ((lex-info (lexem-info obj)))
    `((,(lexer:lexem-line lex-info)
       ,(lexer:lexem-column lex-info))
      (,(lexer:lexem-line lex-info)
       ,(+ (lexer:lexem-column lex-info)
           (length (lexer:lexem-string lex-info)))))))

(defgeneric traverse-and-compare (obj trav-obj nodes-prefix)
  (:method (obj trav-obj nodes-prefix)
    (error "Unsupported traverse-and-compare for ~S" trav-obj)))

(defmethod traverse-and-compare :around (obj trav-obj nodes-prefix)
  (log:debug "In traverse-and-compare for type ~S:~%" (type-of trav-obj))
  (log:debug "obj = ~S~%"  obj)
  (log:debug "trav-obj = ~S~%" trav-obj)
  (log:debug "nodes-prefix = ~S~%~%" nodes-prefix)
  (call-next-method)) 

(defgeneric colapse-node (obj)
  (:method (obj)
    (error "can't collapse-node for ~S" obj)))

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

(defgeneric set-diff-status (obj status &key no-push)
  (:method (obj1 obj2 &key no-push)
    (declare (ignore no-push))
    (format t "set-diff-statud default method for object without diff-status-mixin. Something is wrong: ~A ~A~%" obj1 obj2)
    (error "a")))

(defmethod set-diff-status ((obj main-fields-mixin) status &key no-push)
  (setf (diff-status obj) status)
  (unless no-push
    (cond ((eq status :maybe-new)
           (push obj *maybe-new-nodes*))
          ((eq status :maybe-deleted)
           (push obj *maybe-deleted-nodes*))))
  (setf *was-modified* t))

(defmethod set-diff-status ((obj vector) status &key no-push)
  (loop :for el :across obj
        :do
           (set-diff-status el status :no-push no-push)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
             ;(print lcs-array)
             (labels
                 ((%go-in-one-direction (i j direction)
                  ;  (format t "i = ~S j = ~S ~S~%" i j direction)
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
                  ;  (format t "i = ~S j = ~S~%" i j)
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

(defmethod traverse-and-compare (obj (vec vector) nodes-prefix)
  (loop :for el :across vec
        :do (unless (eq (diff-status el) :maybe-match)
              (when (compare obj el)
                (add-to-maybe-match-list obj el nodes-prefix)
                (return-from traverse-and-compare t))
              (when (traverse-and-compare obj el (cons (id el) nodes-prefix))
                (return-from traverse-and-compare t))))
  nil)

(defmethod apply-diff-patch ((obj vector) diff-patch diff-status)
  (loop :for el :across obj
        :do
           (awhen (gethash (id el) diff-patch)
             (if (eq t it)
                 (set-diff-status el diff-status)
                 (apply-diff-patch el it diff-status)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compare ((obj1 function-call-node) (obj2 function-call-node))
  (let ((equal-leaf-nodes-count 0)
        (is-equal t)
        (diff-patch1)
        (diff-patch2))
    (if (compare (func-lexem obj1)
                 (func-lexem obj2))
        (incf equal-leaf-nodes-count)
        (progn
          (return-from compare (values nil 0 (id obj1) (id obj2)))))
    (multiple-value-bind (is= leaf-counts diff1 diff2)
        (compare (func-arg-forms obj1)
                 (func-arg-forms obj2))
      (incf equal-leaf-nodes-count leaf-counts)
      (unless is=
        (when (func-arg-forms obj1)
          (setf diff-patch1
                (acons :func-arg-forms
                       (if (> leaf-counts 0)
                           diff1
                           t)
                       diff-patch1)))
        (when (func-arg-forms obj2)
          (setf diff-patch2
                (acons :func-arg-forms
                       (if (> leaf-counts 0)
                           diff2
                           t)
                       diff-patch2))))
      (setf is-equal (and is-equal is=)))
    (values is-equal equal-leaf-nodes-count diff-patch1 diff-patch2)))

(defmethod apply-diff-patch ((obj function-call-node) diff-patch diff-status)
  (when (assoc :func-lexem diff-patch)
    (set-diff-status (func-lexem obj) diff-status))
  (awhen (assoc :func-arg-forms diff-patch)
    (if (eq t (rest it))
        (set-diff-status (func-arg-forms obj) diff-status)
        (apply-diff-patch (func-arg-forms obj) (rest it) diff-status))))

(defmethod traverse-and-compare (obj (trav-obj function-call-node) nodes-prefix)
  (traverse-and-compare obj (func-arg-forms trav-obj) nodes-prefix))

(defmethod colapse-node ((node function-call-node))
   (map 'list #'identity (func-arg-forms node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compare ((obj1 list-node) (obj2 list-node))
  (unless (eq (type-of obj1) (type-of obj2))
    (values nil 0 (id obj1) (id obj2)))
  (if (or (elements obj1)
          (elements obj2))
      (compare (elements obj1)
               (elements obj2))
      (values t 1)))

(defmethod apply-diff-patch ((obj list-node) diff-patch diff-status)
  (apply-diff-patch (elements obj) diff-patch diff-status))

(defmethod traverse-and-compare (obj (trav-obj list-node) nodes-prefix)
  (traverse-and-compare obj (elements trav-obj) nodes-prefix))

(defmethod colapse-node ((node list-node))
  (map 'list #'identity (elements node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compare ((obj1 atom-node) (obj2 atom-node))
  (unless (eq (type-of obj1) (type-of obj2))
    (values nil 0 (id obj1) (id obj2)))
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

(defmethod traverse-and-compare (obj (trav-obj atom-node) nodes-prefix)
  (declare (ignore nodes-prefix obj trav-obj))
  nil)

(defmethod colapse-node ((node atom-node))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compare ((obj1 quote-node) (obj2 quote-node))
  (compare (q-s-expr obj1) (q-s-expr obj2)))

(defmethod colapse-node ((node quote-node))
  (list (q-s-expr node)))

(defmethod apply-diff-patch ((obj quote-node) diff-patch diff-status)
  (apply-diff-patch (q-s-expr obj) diff-patch diff-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compare ((obj1 let-node) (obj2 let-node))
  (let ((equal-leaf-nodes-count 1)
        (is-equal t)
        (diff-patch1)
        (diff-patch2))
    (multiple-value-bind (is= leaf-counts diff1 diff2)
        (compare (bindings obj1)
                 (bindings obj2))
      (incf equal-leaf-nodes-count leaf-counts)
      (unless is=
        (setf diff-patch1
              (acons :bindings
                     (if (> leaf-counts 0)
                         diff1
                         t)
                     diff-patch1)
              diff-patch2
              (acons :bindings
                     (if (> leaf-counts 0)
                         diff2
                         t)
                     diff-patch2))
        (setf is-equal (and is-equal is=)))
      (multiple-value-bind (is= leaf-counts diff1 diff2)
          (compare (body-forms obj1)
                   (body-forms obj2))
        (incf equal-leaf-nodes-count leaf-counts)
        (unless is=
          (setf diff-patch1
                (acons :body-forms
                       (if (> leaf-counts 0)
                           diff1
                           t)
                       diff-patch1)
                diff-patch2
                (acons :body-forms
                       (if (> leaf-counts 0)
                           diff2
                           t)
                       diff-patch2))
          (setf is-equal (and is-equal is=)))))
    (values is-equal equal-leaf-nodes-count diff-patch1 diff-patch2)))

(defmethod colapse-node ((node let-node))
  (list* (bindings node)
         (body-forms node)))

(defmethod apply-diff-patch ((obj let-node) diff-patch diff-status)
  (awhen (assoc :bindings  diff-patch)
    (if (eq t (rest it))
        (set-diff-status (bindings obj) diff-status)
        (apply-diff-patch (bindings obj) (rest it) diff-status)))
  (awhen (assoc :body-forms diff-patch)
    (if (eq t (rest it))
        (set-diff-status (body-forms obj) diff-status)
        (apply-diff-patch (body-forms obj) (rest it) diff-status))))

(defmethod traverse-and-compare (obj (trav-obj let-node) nodes-prefix)
  (unless (eq (diff-status trav-obj) :maybe-match)
    (or (when (compare obj (bindings trav-obj))
          (add-to-maybe-match-list obj (bindings trav-obj) nodes-prefix)
          t)
        (traverse-and-compare obj
                              (bindings trav-obj)
                              (cons (id (bindings trav-obj)) nodes-prefix))
        (traverse-and-compare obj (body-forms trav-obj) nodes-prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compare ((obj1 let-binding-node) (obj2 let-binding-node))
  (let ((equal-leaf-nodes-count 0)
        (is-equal t)
        (diff-patch1)
        (diff-patch2))
    (if (compare (var-atom obj1)
                 (var-atom obj2))
        (incf equal-leaf-nodes-count)
        (progn
          (return-from compare (values nil 0 (id obj1) (id obj2)))))
    (cond
      ((and (null (value-s-expr obj1))
            (null (value-s-expr obj2)))
       nil)
      ((and (null (value-s-expr obj1))
            (value-s-expr obj2))
       (setf diff-patch2
             (acons :value-s-expr
                    t
                    diff-patch2))
       (return-from compare (values nil equal-leaf-nodes-count nil diff-patch2)))
      ((and (value-s-expr obj1)
            (null (value-s-expr obj2)))
       (setf diff-patch1
             (acons :value-s-expr
                    t
                    diff-patch1))
       (return-from compare (values nil equal-leaf-nodes-count diff-patch1 nil)))
      (t
       (multiple-value-bind (is= leaf-counts diff1 diff2)
           (compare (value-s-expr obj1)
                    (value-s-expr obj2))
         (incf equal-leaf-nodes-count leaf-counts)
         (unless is=
           (setf diff-patch1
                 (acons :value-s-expr
                        (if (> leaf-counts 0)
                            diff1
                            t)
                        diff-patch1)
                 diff-patch2
                 (acons :value-s-expr
                        (if (> leaf-counts 0)
                            diff2
                            t)
                        diff-patch2)))
         (setf is-equal (and is-equal is=)))))
    (values is-equal equal-leaf-nodes-count diff-patch1 diff-patch2)))

(defmethod colapse-node ((node let-binding-node))
  (list (var-atom node)
        (value-s-expr node)))

(defmethod apply-diff-patch ((obj let-binding-node) diff-patch diff-status)
  (awhen (assoc :value-s-expr diff-patch)
    (if (eq t (rest it))
        (set-diff-status (value-s-expr obj) diff-status)
        (apply-diff-patch (value-s-expr obj) (rest it) diff-status))))

(defmethod traverse-and-compare (obj (trav-obj let-binding-node) nodes-prefix)
  (unless (eq (diff-status trav-obj) :maybe-match)
    (or (when (compare obj (var-atom trav-obj))
          (add-to-maybe-match-list obj (var-atom trav-obj) nodes-prefix)
          t)
        (when (value-s-expr trav-obj)
          (when (compare obj (value-s-expr trav-obj))
            (add-to-maybe-match-list obj (value-s-expr trav-obj) nodes-prefix)
            t)
          (traverse-and-compare obj
                                (value-s-expr trav-obj)
                                (cons (id (value-s-expr trav-obj))
                                      nodes-prefix))))))

;;;defparameter
(defmethod compare ((obj1 defparameter-node) (obj2 defparameter-node))
  (multiple-value-bind (is-equal eq-leaf-counts diff1 diff2)
      (compare
       (value-s-expr obj1)
       (value-s-expr obj2))
    (declare (ignore eq-leaf-counts))
    (unless is-equal
      (apply-diff-patch (value-s-expr obj1) diff1 :maybe-deleted)
      (apply-diff-patch (value-s-expr obj2) diff2 :maybe-new))))


;;;if-node
(defmethod compare ((obj1 if-node) (obj2 if-node))
  (let ((equal-leaf-nodes-count 0)
        (is-equal t)
        (diff-patch1)
        (diff-patch2))
    (multiple-value-bind (is= leaf-counts diff1 diff2)
        (compare (test-s-expr obj1)
                 (test-s-expr obj2))
      (incf equal-leaf-nodes-count leaf-counts)
      (unless is=
        (setf diff-patch1
              (acons :test-s-expr
                     (if (> leaf-counts 0)
                         diff1
                         t)
                     diff-patch1)
              diff-patch2
              (acons :test-s-expr
                     (if (> leaf-counts 0)
                         diff2
                         t)
                     diff-patch2))
        (setf is-equal (and is-equal is=)))
      (multiple-value-bind (is= leaf-counts diff1 diff2)
          (compare (then-s-expr obj1)
                   (then-s-expr obj2))
        (incf equal-leaf-nodes-count leaf-counts)
        (unless is=
          (setf diff-patch1
                (acons :then-s-expr
                       (if (> leaf-counts 0)
                           diff1
                           t)
                       diff-patch1)
                diff-patch2
                (acons :then-s-expr
                       (if (> leaf-counts 0)
                           diff2
                           t)
                       diff-patch2))
          (setf is-equal (and is-equal is=)))
        (cond
          ((and (null (else-s-expr obj1))
                (null (else-s-expr obj2)))
           nil)
          ((and (null (else-s-expr obj1))
                (else-s-expr obj2))
           (setf diff-patch2
                 (acons :else-s-expr
                        t
                        diff-patch2)))
          ((and (else-s-expr obj1)
                (null (else-s-expr obj2)))
           (setf diff-patch1
                 (acons :else-s-expr
                        t
                        diff-patch1)))
          (t
           (multiple-value-bind (is= leaf-counts diff1 diff2)
               (compare (else-s-expr obj1)
                        (else-s-expr obj2))
             (incf equal-leaf-nodes-count leaf-counts)
             (unless is=
               (setf diff-patch1
                     (acons :else-s-expr
                            (if (> leaf-counts 0)
                                diff1
                                t)
                            diff-patch1)
                     diff-patch2
                     (acons :else-s-expr
                            (if (> leaf-counts 0)
                                diff2
                                t)
                            diff-patch2)))
             (setf is-equal (and is-equal is=)))))))
    (values is-equal equal-leaf-nodes-count diff-patch1 diff-patch2)))

(defmethod collapse-node ((node if-node))
  (list
   (test-s-expr node)
   (then-s-expr node)
   (else-s-expr node)))

(defmethod apply-diff-patch ((obj if-node) diff-patch diff-status)
  (awhen (assoc :then-s-expr diff-patch)
    (if (eq t (rest it))
        (set-diff-status (then-s-expr obj) diff-status)
        (apply-diff-patch (then-s-expr obj) (rest it) diff-status)))
  (awhen (assoc :test-s-expr diff-patch)
    (if (eq t (rest it))
        (set-diff-status (test-s-expr obj) diff-status)
        (apply-diff-patch (test-s-expr obj) (rest it) diff-status)))
  (awhen (assoc :else-s-expr diff-patch)
    (if (eq t (rest it))
        (set-diff-status (else-s-expr obj) diff-status)
        (apply-diff-patch (else-s-expr obj) (rest it) diff-status))))

(defmethod traverse-and-compare (obj (trav-obj if-node) nodes-prefix)
  (unless (eq (diff-status trav-obj) :maybe-match)
    (or (when (compare obj (test-s-expr trav-obj))
          (add-to-maybe-match-list obj (test-s-expr trav-obj) nodes-prefix)
          t)
        (traverse-and-compare obj
                              (test-s-expr trav-obj)
                              nodes-prefix)
        (when (compare obj (then-s-expr trav-obj))
          (add-to-maybe-match-list obj (then-s-expr trav-obj) nodes-prefix)
          t)
        (traverse-and-compare obj
                              (then-s-expr trav-obj)
                              nodes-prefix)
        (when (else-s-expr trav-obj)
          (when (compare obj (else-s-expr trav-obj))
            (add-to-maybe-match-list obj (else-s-expr trav-obj) nodes-prefix)
            t)
          (traverse-and-compare obj
                                (else-s-expr trav-obj)
                                nodes-prefix)))))
