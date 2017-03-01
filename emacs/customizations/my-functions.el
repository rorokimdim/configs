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
  (dolist (prefix '("*Messages*"
                    "*nrepl-server"
                    "*racket-command-output*"
                    "*Backtrace*"
                    "*Completions*"
                    "*Geiser dbg*"))
    (when (string-prefix-p prefix (buffer-name))
      (funcall switcher))))

(defun my-get-sanitized-projectile-buffer-names ()
  "Gets list of sorted projectile buffer names without the non-file-or-process buffers."
  (sort (mapcar #'buffer-name (projectile-buffers-with-file-or-process (projectile-project-buffers))) 'string<))

(defun my-inside-projectile-project? ()
  "Checks if we are inside a projectil project."
   (not (string= "-" (projectile-project-name))))

(defun my-projectile-buffer-toggle ()
  "Toggles projectile buffers."
  (interactive)
  (if (my-inside-projectile-project?)
    (let* ((buffer-name-list (my-get-sanitized-projectile-buffer-names))
           (i (cl-position (buffer-name) buffer-name-list)))
      (if i
        (switch-to-buffer (nth (mod (+ i 1) (length buffer-name-list)) buffer-name-list))
        (call-interactively 'ido-switch-buffer)))
    (call-interactively 'ido-switch-buffer)))

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

(require 'neotree)
(defun my-neotree-projectile-project-dir ()
    "Opens neotree using the projectile project root."
    (interactive)
    (if (my-inside-projectile-project?)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)))
          (message "Could not find project root.")))
      (if (and (buffer-file-name) (not  (neo-global--window-exists-p)))
        (progn
            (neotree-find (buffer-file-name))
            (neotree-show))
        (neotree-toggle))))
