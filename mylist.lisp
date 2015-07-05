(defclass node ()
  ((data :accessor node-data
         :initform nil
         :initarg :data)

   (next :accessor node-next
         :initform nil
         :initarg :next)))

(defclass mylist ()
  ((head :accessor mylist-head
         :initform (make-instance 'node)
         :initarg :head)
   
   (end :accessor mylist-end
        :initform nil
        :initarg :end)))

(defmethod initialize-instance :after
  ((c mylist) &rest args)
  (setf (mylist-end c) (mylist-head c)))

(defmethod mylist-append
  ((self mylist) &rest data)
  (defun add-data (data-list)
    (setf (node-data (mylist-end self)) (car data-list))
    (setf (node-next (mylist-end self)) (make-instance 'node))
    (setf (mylist-end self) (node-next (mylist-end self)))
    (if (equal (cdr data-list) nil)
      nil
      (add-data (cdr data-list))))
  (add-data data)
  self)

(defmethod print-object ((self node) out)
  (print-unreadable-object (self out :type t)
    (format out "~s" (node-data self))))

(defmethod print-object ((self mylist) out)
  (print-unreadable-object (self out :type t)
    (defun print-node (somenode)
      (if (equal (node-next somenode) nil)
        nil
        (progn
          (format out "~s" (node-data somenode))
          (if (equal (node-next (node-next somenode)) nil)
            nil
            (format out  ","))
          (print-node (node-next somenode)))))
    (print-node (mylist-head self))))

(defmethod mylist-len
  ((self mylist))
   (defun node-len(somenode)
     (if (equal (node-next somenode) nil)
       0
       (+ 1 (node-len (node-next somenode)))))
   (node-len (mylist-head self)))
