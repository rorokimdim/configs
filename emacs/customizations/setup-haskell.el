(setq lsp-haskell-process-path-hie "hie-wrapper")

(setq haskell-stylish-on-save t)
(setq haskell-mode-stylish-haskell-path "brittany")

(require 'lsp)
(require 'lsp-haskell)

(add-hook 'haskell-mode-hook #'lsp)
