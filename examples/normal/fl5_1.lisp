(defun k1 ()
  (f 1))

;fun c
(defun c () (* 2 2))

(defun del-func (v) v)

(defun b ()
   (d 9) 
   (wrap1 (f 2))
   (let ((a 6)
         (b 9)
         (c 12))
        (if (* a b)
            (k1)
            (fun2 123)))
  (f 3)
  (c)
  (when (atom 1)
    (print "Good day!")))