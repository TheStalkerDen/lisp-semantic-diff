;fun k
(defun k1 ()
  (f 1))

(defun c () (* 2 2))

(defun b () 
   (r 1)
   (let ((a 6)
         (b 9))
        (f 2)
        (if (* a b)
            (k1)
            (fun2 123)
        (f 3)))
  (when (atom 1)
    (print "Good day!")
  (d 2)))