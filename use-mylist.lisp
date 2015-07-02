(load 'mylist.lisp)
(setq a-list (make-instance 'mylist))
(mylist-append a-list "the" "answer" "is" 42)
(princ a-list)
