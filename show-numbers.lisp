(setq n 1)
(loop
  (if (listen *terminal-io*) (return (read-line)))
  (print n)
  (setq n (+ n 1))
  (sleep 1))

