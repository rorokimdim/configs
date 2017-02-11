(defun my-eval-and-replace ()
  "Replace preceding sexp with its value."
  (interactive)
  (forward-char)
  (backward-kill-sexp)
  (condition-case nil
                  (prin1 (eval (read (current-kill 0)))
                         (current-buffer))
                  (error (message "Invalid expression")
                         (insert (current-kill 0)))))

(defun my-eval-print-last-sexp ()
  "Make eval-print-last-sexp work in evil mode."
  (interactive)
  (forward-char)
  (eval-print-last-sexp))

(defun my-buffer-toggle (switcher)
  "Toggles buffer using SWITCHER function, skipping over any useless buffers."
  (interactive)
  (funcall switcher)
  (dolist (prefix '("*Messages*"
                    "*nrepl-server"
                    "*Completions*"))
    (when (string-prefix-p prefix (buffer-name))
      (funcall switcher))))
