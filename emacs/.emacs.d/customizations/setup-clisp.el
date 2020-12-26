(when (file-exists-p "~/.roswell/helper.el")
  (load (expand-file-name "~/.roswell/helper.el"))

  (setq inferior-lisp-program "ros -Q run")
  (setq slime-contribs '(slime-fancy))

  ;; Shortcuts for slime repl
  (define-key slime-repl-mode-map (kbd "<up>") 'slime-repl-previous-input)
  (define-key slime-repl-mode-map (kbd "<down>") 'slime-repl-next-input)
  (define-key slime-repl-mode-map (kbd "C-l") 'slime-repl-clear-buffer)

  ;; Shortcuts for common lisp mode
  (require 'bind-map)
  (bind-map my/clisp-mode-map
    :keys ("s-,")
    :evil-keys (",")
    :evil-states (normal visual)
    :major-modes (lisp-mode)
    :bindings ("ee" (lambda ()
                      (interactive)
                      (forward-char)
                      (slime-eval-last-expression))
               "ef" 'slime-eval-defun
               "eb" 'slime-eval-buffer
               "er" 'slime-eval-region
               "r"  'slime-repl)))


