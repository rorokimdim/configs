(setq inferior-lisp-program "sbcl")
(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

;; Shortcuts for common lisp mode
(require 'bind-map)
(bind-map my-clisp-mode-map
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
             "r"  'slime-repl))

;; Shortcuts for slime repl
(define-key slime-repl-mode-map (kbd "<up>") 'slime-repl-previous-input)
(define-key slime-repl-mode-map (kbd "<down>") 'slime-repl-next-input)
(define-key slime-repl-mode-map (kbd "C-l") 'slime-repl-clear-buffer)
