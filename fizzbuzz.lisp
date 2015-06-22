(defun fizzbuzz (n end) (if (> n end) nil (cons
	(cond
	((= 0 (mod n 15)) (print "fizzbuzz"))
	((= 0 (mod n 3)) (print "fizz"))
	((= 0 (mod n 5)) (print "buzz"))
	(t (print n))) (fizzbuzz (+ n 1) end))))

(print (fizzbuzz 1 100))
