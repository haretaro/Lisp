(defun fizzbuzz (n end) (if (> n end) nil (cons
	(cond
		((= 0 (mod n 15)) "fizzbuzz")
		((= 0 (mod n 3))  "fizz")
		((= 0 (mod n 5)) "buzz")
		(t n))
	(fizzbuzz (+ n 1) end))))

(format t "~{~A ~}" (fizzbuzz 1 100))
