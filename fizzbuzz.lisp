(defun fizzbuzz (n)
	(cond ((= 0 (mod n 15)) (print "fizzbuzz"))
		((= 0 (mod n 3)) (print "fizz"))
		((= 0 (mod n 5)) (print "buzz"))
		(t (print n))))

(defun show (n)
	(when (< n 101)
		(fizzbuzz n)
		(show (+ n 1))))

(show 1)

