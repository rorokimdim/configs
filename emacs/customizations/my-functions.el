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

(defun my-shell-command-to-buffer ()
  "Runs a shell command and appends output to current buffer starting from current point."
  (interactive)
  (with-current-buffer (current-buffer)
    (insert (shell-command-to-string (read-string "Enter command: ")))))

(defun my-buffer-toggle (switcher)
  "Toggles buffer using SWITCHER function, skipping over any useless buffers."
  (interactive)
  (funcall switcher)
  (when (string= "*terminal*" (buffer-name))
    (call-interactively 'end-of-buffer))
  (dolist (prefix '("*Messages*"
                    "*Compile-Log*"
                    "*nrepl-server"
                    "*racket-command-output*"
                    "*scratch*"
                    "*Backtrace*"
                    "*Completions*"
                    "*Geiser dbg*"))
    (when (string-prefix-p prefix (buffer-name))
      (my-buffer-toggle switcher))))

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

(defun my-python-add-breakpoint ()
  "Adds ipbp trace."
  (interactive)
  (forward-char)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defvar my-workspace-directory
  "~/workspace/"
  "Path to my workspace directory")
(defun my-find-file ()
  "Defines a custom find-file function."
  (interactive)
  (cd (read-directory-name "Directory: " (or (ffip-project-root) my-workspace-directory)))
  (call-interactively 'find-file-in-repository))
