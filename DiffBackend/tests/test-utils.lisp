(uiop:define-package :diff-backend/tests/test-utils
    (:use :cl :diff-backend/nodes)
  (:export #:conv-for-cmp-test))

(in-package :diff-backend/tests/test-utils)

(defgeneric conv-for-cmp-test (obj))

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
              (,(conv-for-cmp-test parameters-list))
              ,@(conv-for-cmp-test body-forms)))))

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
