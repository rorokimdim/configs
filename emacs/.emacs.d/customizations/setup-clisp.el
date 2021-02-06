(defun my/slime-repl ()
  "Starts slime if not started and switch to slime repl."
  (interactive)
  (unless (get-process "SLIME Lisp")
    (slime))
  (slime-repl))

(when (file-exists-p "~/.roswell/helper.el")
  (load (expand-file-name "~/.roswell/helper.el"))

  (setq inferior-lisp-program "ros -Q run")
  (setq slime-contribs '(slime-fancy))

  ;; Shortcuts for slime repl
  (define-key slime-repl-mode-map (kbd "<up>") 'slime-repl-previous-input)
  (define-key slime-repl-mode-map (kbd "<down>") 'slime-repl-next-input)
  (define-key slime-repl-mode-map (kbd "C-l") 'slime-repl-clear-buffer)

  (add-hook 'lisp-mode-hook 'highlight-defined-mode)
  (add-hook 'lisp-mode-hook 'lisp-extra-font-lock-mode)

  ;; Shortcuts for common lisp mode
  (require 'bind-map)
  (bind-map my/clisp-mode-map
    :keys ("s-,")
    :evil-keys (",")
    :evil-states (normal visual)
    :major-modes (lisp-mode)
    :bindings ("ee" 'slime-eval-last-expression
               "ef" 'slime-eval-defun
               "eb" 'slime-eval-buffer
               "er" 'slime-eval-region
               "gd" 'slime-edit-definition
               "hh" 'slime-documentation
               "hb" 'slime-documentation-lookup
               "r" 'my/slime-repl)))
