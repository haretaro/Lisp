(defmacro swap (a b)
  (let ((temp (gensym)))
    `(progn
       (setf,temp,a)
       (setf,a,b)
       (setf,b,a))))

(setq a 3.14)
(setq p 42)
(swap a p)
(print p)
(rotatef a p)
(print p)
