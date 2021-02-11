(uiop:define-package :diff-backend/statistics
    (:nicknames :statistics)
  (:use :cl)
  (:export))

(in-package :diff-backend/statistics)

(defparameter *file-ver-1-stats* nil)
(defparameter *file-ver-2-stats* nil)

(defun init-stats ()
  (setf *file-ver-1-stats* (make-hash-table :test #'equal))
  (setf *file-ver-2-stats* (make-hash-table :test #'equal)))
