(defun read-numbers ()
  (progn
    (let (str))
    (setq str (read-line))
    (if (string= str "")
      nil
      (cons (parse-integer str) (read-numbers)))))

(print (read-numbers))
