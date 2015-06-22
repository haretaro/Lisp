(defun numlist (n end) (if (> n end) nil (cons n (numlist (+ n 1) end))))
(print (numlist 1 5))
