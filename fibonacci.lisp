(defun fibonacci (end memo)
  (if (> 2 (length memo))
    (fibonacci end
               (cons (print 1) memo))
    (if (< end (length memo))
      memo
      (fibonacci end
                 (cons (print
                         (+ (car memo)
                            (car (cdr memo))
                            )) memo)))))

(fibonacci 1000 nil)
