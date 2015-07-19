(defclass node ()
  ((data :accessor node-data
         :initform nil
         :initarg :data)

   (next :accessor node-next
         :initform nil
         :initarg :next)))

(defclass mylist ()
  ((head :accessor mylist-start
         :initform (make-instance 'node)
         :initarg :head)
   
   (end :accessor mylist-end
        :initform nil
        :initarg :end)))

(defmethod initialize-instance :after
  ((c mylist) &rest args)
  (setf (mylist-end c) (mylist-start c)))

(defmethod mylist-append
  ((self mylist) &rest data)
  (defun add-data (data-list)
    (setf (node-data (mylist-end self)) (car data-list))
    (setf (node-next (mylist-end self)) (make-instance 'node))
    (setf (mylist-end self) (node-next (mylist-end self)))
    (if (cdr data-list)
      (add-data (cdr data-list))
      nil))
  (add-data data)
  self)

(defmethod print-object ((self node) out)
  (print-unreadable-object (self out :type t)
    (format out "~s" (node-data self))))

(defmethod print-object ((self mylist) out)
  (print-unreadable-object (self out :type t)
    (defun print-node (somenode)
      (if (node-next somenode)
        (progn
          (format out "~s" (node-data somenode))
          (if (node-next (node-next somenode))
            (format out  ",")
            nil)
          (print-node (node-next somenode)))
        nil))
    (print-node (mylist-start self))))

(defmethod mylist-len
  ((self mylist))
   (defun node-len(somenode)
     (if (node-next somenode)
       (+ 1 (node-len (node-next somenode)))
       0))
   (node-len (mylist-start self)))

(defmethod mylist-get
  ((self mylist) index)
  (defun getdata(somenode num)
    (if (= num 0)
      (node-data somenode)
      (getdata (node-next somenode) (- num 1))))
  (getdata (mylist-start self) index))

(defmethod mylist-clone ((self mylist))
  (let (newlist)
    (setq newlist (make-instance 'mylist))
    (setq node (mylist-start self))
    (loop
      (if (node-next node)
        (progn
          (mylist-append newlist (node-data node))
          (setq node (node-next node)))
        (return newlist)))))

(defmethod mylist-extend ((self mylist) (li mylist))
  (let (newlist node)
    (setq newlist (mylist-clone self))
    (setq node (mylist-start li))
    (loop
      (if (node-next node)
        (progn
          (mylist-append newlist (node-data node))
          (setq node (node-next node)))
        (return newlist)))))

(defmethod mylist-tail ((self mylist))
  (let (newlist)
    (setq newlist (mylist-clone self))
    (setf mylist-start (node-next (mylist-start newlist)))
    newlist))

(defmethod mylist-reverse ((self mylist))
  (mylist-append (mylist-reverse (mylist-tail self)) (mylist-start self)))
