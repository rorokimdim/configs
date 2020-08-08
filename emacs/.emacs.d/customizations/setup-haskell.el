(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(with-eval-after-load 'haskell-mode
  (setq haskell-stylish-on-save t)
  (setq haskell-mode-stylish-haskell-path "brittany")
  (subword-mode +1)
  (eldoc-mode +1)
  (haskell-indentation-mode +1)
  (interactive-haskell-mode +1)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch))

(with-eval-after-load 'haskell-interactive-mode
  (define-key haskell-interactive-mode-map (kbd "<up>") 'haskell-interactive-mode-history-previous)
  (define-key haskell-interactive-mode-map (kbd "<down>") 'haskell-interactive-mode-history-next)
  (define-key haskell-interactive-mode-map (kbd "C-c C-q") 'haskell-interactive-kill)
  (define-key haskell-interactive-mode-map (kbd "C-l") 'haskell-interactive-mode-clear))

(require 'bind-map)
(bind-map my-haskell-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (haskell-mode)
  :bindings (
             "if" 'haskell-mode-format-imports
             "ii" 'haskell-add-import
             "gd" 'haskell-mode-jump-to-def
             "hd" 'haskell-describe
             "hh" 'haskell-hoogle))

(custom-set-variables
 '(haskell-process-show-debug-tips nil)
 '(haskell-interactive-popup-errors nil)
 '(haskell-compile-cabal-build-command "stack build")
 '(haskell-process-type 'stack-ghci))

(require 'company-ghci)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook 'company-mode)

(require 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

(add-hook 'haskell-interactive-mode-hook 'company-mode)
