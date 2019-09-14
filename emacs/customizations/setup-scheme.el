(setq geiser-active-implementations '(chicken))

;; Shortcuts for scheme mode
(require 'bind-map)
(bind-map my-scheme-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (scheme-mode)
  :bindings ("r" 'switch-to-geiser
             "gm" 'geiser-doc-look-up-manual
             "ge" 'geiser-expand-definition
             "eb" 'geiser-eval-buffer
             "ee" 'geiser-eval-last-sexp
             "ef" 'geiser-eval-definition
             "er" 'geiser-eval-region))

(with-eval-after-load 'geiser-repl
  (define-key geiser-repl-mode-map (kbd "C-l") 'geiser-repl-clear-buffer)
  (define-key geiser-repl-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key geiser-repl-mode-map (kbd "<down>") 'comint-next-input))
