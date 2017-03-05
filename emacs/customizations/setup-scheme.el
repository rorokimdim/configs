;; Setup geiser
(setq geiser-active-implementations '(racket))

;; Shortcuts for scheme mode
(require 'evil-leader)
(evil-leader/set-key-for-mode 'scheme-mode
  "r" (lambda ()
        (interactive)
        (geiser-mode-switch-to-repl-and-enter)
        (delete-other-windows)
        (geiser-repl-clear-buffer))
  "eb" 'geiser-eval-buffer
  "ee" 'geiser-eval-last-sexp
  "ef" 'geiser-eval-definition
  "er" 'geiser-eval-region
  "gm" 'geiser-doc-look-up-manual)
