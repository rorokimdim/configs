(setq lsp-haskell-process-path-hie "hie-wrapper")

(setq haskell-stylish-on-save t)
(setq haskell-mode-stylish-haskell-path "brittany")

(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))

(require 'lsp)
(require 'lsp-haskell)

(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'haskell-mode-hook #'lsp)
