(defparameter memo (make-hash-table))

(defun fibonacci (n)
  (if (gethash n memo)
    (gethash n memo)
    (if (> 3 n)
      (setf (gethash n memo) 1)
      (setf (gethash n memo)
            (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))

(setq n 1)
(loop
  (print (fibonacci n))
  (setq n (+ n 1))
  (if (> n 1000) (return t)))
