(uiop:define-package :diff-backend/statistics
    (:nicknames :statistics)
  (:use :cl)
  (:export #:init-stats
           #:add-to-stats
           #:get-stats
           #:gen-classified-results
           #:gen-stats-ht-for-json))

(in-package :diff-backend/statistics)

(defparameter *file-ver-1-stats* nil)
(defparameter *file-ver-2-stats* nil)

(declaim (optimize (debug 3)))

(defun init-stats ()
  (setf *file-ver-1-stats* (make-hash-table :test #'eq))
  (setf *file-ver-2-stats* (make-hash-table :test #'eq)))

(defun add-to-stats (name obj &key stat-name file-ver)
  (alexandria:eswitch (file-ver)
    (1
     (unless (gethash stat-name *file-ver-1-stats*)
       (setf (gethash stat-name *file-ver-1-stats*)
             (make-hash-table :test #'equal)))
     (setf (gethash name (gethash stat-name *file-ver-1-stats*))
           `(,obj :no-mod)))
    (2
     (unless (gethash stat-name *file-ver-2-stats*)
       (setf (gethash stat-name *file-ver-2-stats*)
             (make-hash-table :test #'equal)))
     (setf (gethash name (gethash stat-name *file-ver-2-stats*))
           `(,obj :no-mod)))))

(defun get-stats (ver)
  (alexandria:eswitch (ver)
                      (1 *file-ver-1-stats*)
                      (2 *file-ver-2-stats*)))

(defun gen-classified-results (file-ver)
  (let ((stats-ht (alexandria:eswitch (file-ver)
                    (1 *file-ver-1-stats*)
                    (2 *file-ver-2-stats*)))
        (result))
    (maphash (lambda (stat-name names-ht)
               (push
                `(,stat-name ,@(filter-names-ht names-ht))
                result))
             stats-ht)
    (reverse result)))

(defun filter-names-ht (names-ht)
  (let (no-mod-names
        mod-names
        deleted-names
        new-names)
    (maphash (lambda (name val)
               (ecase (second val)
                 (:no-mod
                  (push name no-mod-names))
                 (:modified
                  (push name mod-names))
                 (:deleted
                  (push name deleted-names))
                 (:new
                  (push name new-names ))))
             names-ht)
    (remove nil
            `(,(when no-mod-names
                 `(:no-mod
                   ,(reverse no-mod-names)))
               ,(when mod-names
                  `(:modified
                    ,(reverse mod-names)))
               ,(when deleted-names
                  `(:deleted
                    ,(reverse deleted-names)))
               ,(when new-names
                  `(:new
                    ,(reverse new-names)))))))

(defun gen-stats-ht-for-json (file-ver)
  (let ((stats-ht (alexandria:eswitch (file-ver)
                    (1 *file-ver-1-stats*)
                    (2 *file-ver-2-stats*)))
        (result))
    (maphash (lambda (stat-name names-ht)
               (push
                `(,stat-name . ,(gen-filter-names-ht names-ht))
                result))
             stats-ht)
    (alexandria:alist-hash-table result :test #'equal)))

(defun gen-filter-names-ht (names-ht)
  (let (no-mod-names
        mod-names
        deleted-names
        new-names)
    (maphash (lambda (name val)
               (let ((n-name (string-upcase name)))
                 (ecase (second val)
                   (:no-mod
                    (push n-name no-mod-names))
                   (:modified
                    (push n-name mod-names))
                   (:deleted
                    (push n-name deleted-names))
                   (:new
                    (push n-name new-names )))))
             names-ht)
    (alexandria:alist-hash-table
     `((:no-mod .
                ,(reverse no-mod-names))
       (:modified .
                  ,(reverse mod-names))
       (:deleted .
                 ,(reverse deleted-names))
       (:new .
             ,(reverse new-names)))
     :test #'equal)))
