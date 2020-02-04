(setq haskell-stylish-on-save t)
(setq haskell-mode-stylish-haskell-path "brittany")

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info))

(with-eval-after-load 'haskell-interactive-mode
  (define-key haskell-interactive-mode-map (kbd "<up>") 'haskell-interactive-mode-history-previous)
  (define-key haskell-interactive-mode-map (kbd "<down>") 'haskell-interactive-mode-history-next)
  (define-key haskell-interactive-mode-map (kbd "C-c C-q") 'haskell-interactive-kill)
  (define-key haskell-interactive-mode-map (kbd "C-l") 'haskell-interactive-mode-clear))

(custom-set-variables
  '(haskell-process-show-debug-tips nil)
  '(haskell-interactive-popup-errors nil)
  '(haskell-process-type 'stack-ghci))

(require 'company-ghci)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook 'company-mode)
(add-hook 'haskell-interactive-mode-hook 'company-mode)
