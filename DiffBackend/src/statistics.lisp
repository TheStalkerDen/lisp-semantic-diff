(uiop:define-package :diff-backend/statistics
    (:nicknames :statistics)
  (:use :cl)
  (:export #:init-stats
           #:add-to-stats))

(in-package :diff-backend/statistics)

(defparameter *file-ver-1-stats* nil)
(defparameter *file-ver-2-stats* nil)

(declaim (optimize (debug 3)))

(defun init-stats ()
  (setf *file-ver-1-stats* (make-hash-table :test #'eq))
  (setf *file-ver-2-stats* (make-hash-table :test #'eq))
  (setf (gethash :defuns *file-ver-1-stats*)
        (make-hash-table :test #'equal))
  (setf (gethash :defuns *file-ver-2-stats*)
        (make-hash-table :test #'equal)))

(defun add-to-stats (name obj &key stat-name file-ver)
  (ecase stat-name
    ((:defuns)
     (alexandria:eswitch (file-ver)
       (1
        (setf (gethash name (gethash :defuns *file-ver-1-stats*))
              `(,obj :no-mod)))
       (2
        (setf (gethash name (gethash :defuns *file-ver-2-stats*))
              `(,obj :no-mod)))))))
