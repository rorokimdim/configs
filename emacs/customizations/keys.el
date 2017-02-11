;; Use F2 to eval-and-replace lisp expressions
(defun eval-and-replace ()
  "Replace preceding sexp with its value."
  (interactive)
  (forward-char)
  (backward-kill-sexp)
  (condition-case nil
                  (prin1 (eval (read (current-kill 0)))
                         (current-buffer))
                  (error (message "Invalid expression")
                         (insert (current-kill 0)))))
(global-set-key [f2] 'eval-and-replace)

;; Use F3 and F4 to switch buffers
(global-set-key [f3] 'previous-buffer)
(global-set-key [f4] 'switch-to-buffer)

;; Use F5 to show buffer list
(global-set-key [f5]
                (lambda ()
                  (interactive)
                  (list-buffers)
                  (delete-other-windows)
                  (switch-to-buffer "*Buffer List*")))

;; Use F9 to kill all buffers
(global-set-key [f9]
                (lambda ()
                  (interactive)
                  (mapc 'kill-buffer (buffer-list))))


