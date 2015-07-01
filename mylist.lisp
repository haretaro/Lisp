(defstruct node
  (data)
  (next))

(defstruct mylist
  (head)
  (end))

(defun mylist-construct ()
  (let (l)
    (progn
      (setq l (make-mylist))
      (setf (mylist-head l) (make-node))
      (setf (mylist-end l) (mylist-head l))
      l)))

(defun mylist-append (self x)
  (setf (node-data (mylist-end self)) x)
  (setf (mylist-end self) (setf (node-next (mylist-end self)) (make-node))))
