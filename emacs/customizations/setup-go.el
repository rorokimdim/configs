(use-package go-mode
  :ensure t
  :init
  :config
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

(require 'bind-map)
(bind-map my-golang-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (go-mode)
  :bindings ("dd" 'lsp-ui-doc-glance
             "gt" 'lsp-find-type-definition
             "gd" 'lsp-find-definition
             "gr" 'lsp-find-references))

(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
