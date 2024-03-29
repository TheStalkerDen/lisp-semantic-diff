(uiop:define-package :diff-backend/utils
    (:nicknames :utils)
  (:use :cl)
  (:export #:defclass*))

(in-package :diff-backend/utils)

(defmacro defclass* (name (&rest superclasses) (&rest slot-specs)
                     &optional class-option)
  (let ((exports (mapcan (lambda (spec)
                           (let ((name (or (getf (cdr spec) :accessor)
                                           (getf (cdr spec) :reader)
                                           (getf (cdr spec) :writer))))
                             (when name (list name))))
                         slot-specs)))
    `(progn
       (defclass ,name (,@superclasses)
         ,@ (remove nil
                    (list (mapcar
                           (lambda (spec)
                             (let ((export-pos (position :export spec)))
                               (if export-pos
                                   (append (subseq spec 0 export-pos)
                                           (subseq spec (+ 2 export-pos)))
                                   spec)))
                           slot-specs)
                          (when class-option class-option))))
       ,@(mapcar (lambda (name) `(export ',name))
                 exports)
       (export ',name))))
