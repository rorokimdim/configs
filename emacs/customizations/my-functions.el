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
                    "*Backtrace*"
                    "*Completions*"))
    (when (string-prefix-p prefix (buffer-name))
      (funcall switcher))))

(defun my-add-pretty-symbols ()
  "Sets my pretty-symbol mappings."
  (setq prettify-symbols-alist
        '(
          ("alpha" . 945)
          ("beta" . 946)
          ("gamma" . 947)
          ("delta" . 948)
          ("epsilon" . 949)
          ("zeta" . 950)
          ("eta" . 951)
          ("theta" . 952)
          ("lambda" . 955)
          ("mu" . 956)
          ("pi" . 960)
          ("phi" . 966)
          ("psi" . 968)
          ("omega" . 969))))
