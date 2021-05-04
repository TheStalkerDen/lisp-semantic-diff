(uiop:define-package :diff-backend/tests/test-utils
    (:use :cl :diff-backend/nodes)
  (:export
   #:deep-equal
   #:conv-for-cmp-test))

(in-package :diff-backend/tests/test-utils)

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
                               (if (deep-equal (slot-value obj1 slot-name)
                                               (slot-value obj2 slot-name))
                                   t
                                   (progn (print obj1)
                                          (print obj2)
                                          nil)))))
            (list  (if (= (length obj1) (length obj2))
                       (every #'deep-equal obj1 obj2)
                       (progn
                         (format t "Length not equal!~%")
                         (format t "Length ~A : ~A~%" (length obj1) obj1)
                         (format t "Length ~A : ~A~%" (length obj2) obj2)
                         nil)))
            (vector (if (= (length obj1) (length obj2))
                       (every #'deep-equal obj1 obj2)
                       (progn
                         (format t "Length not equal!~%")
                         (format t "Length ~A : ~A~%" (length obj1) obj1)
                         (format t "Length ~A : ~A~%" (length obj2) obj2)
                         nil)))
            (t (unless (equalp obj1 obj2)
                 (print obj1)
                 (print obj2)
                 (return-from deep-equal nil))
             t))
          t
          (progn
            (print obj1)
            (print obj2)
            nil))
      (progn (print "Not equal types")
             (format t "Type: ~A obj1: ~A ~%" (type-of obj1) obj1)
             (format t "Type: ~A obj2: ~A ~%" (type-of obj2) obj2)
             nil)))

(defgeneric conv-for-cmp-test (obj))

(defmethod conv-for-cmp-test ((obj vector))
  (loop :for el :across obj
        :collect (conv-for-cmp-test el)))

(defmethod conv-for-cmp-test ((obj list))
  (loop :for el :in obj
        :collect (conv-for-cmp-test el)))

(defmethod conv-for-cmp-test ((obj defun-node))
  (with-slots (diff-status keyword-lexem function-name parameters-list body-forms)
      obj
    (remove nil
            `(,(unless (eq diff-status :same)
                 diff-status)
              ,(conv-for-cmp-test keyword-lexem)
              ,(conv-for-cmp-test function-name)
              (,@(conv-for-cmp-test parameters-list))
              ,@(conv-for-cmp-test body-forms))
            :count 1 :end 1)))

(defmethod conv-for-cmp-test ((obj function-call-node))
  (with-slots (diff-status func-lexem func-arg-forms) obj
    (remove nil
            `(,(unless (eq diff-status :same)
                 diff-status)
              ,(conv-for-cmp-test func-lexem)
              ,@(conv-for-cmp-test func-arg-forms)))))

(defmethod conv-for-cmp-test ((obj list-node))
  (with-slots (elements) obj
    (conv-for-cmp-test elements)))

(defmethod conv-for-cmp-test ((obj lexem-wrapper-node))
  (with-slots (diff-status) obj
    (if (eq diff-status :same)
        :<el>
        `(,diff-status :<el>))))
