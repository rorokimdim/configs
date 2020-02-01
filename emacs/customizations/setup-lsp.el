(use-package lsp-mode :commands lsp :ensure t)
(use-package lsp-ui :commands lsp-ui-mode :ensure t)
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (require 'lsp-mode)
  (require 'lsp-ui)
  ;; (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-eldoc-enable-hover nil)
  ;; (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-ui-sideline-enable nil)
  ;; (setq lsp-eldoc-enable-signature-help nil)
  (push 'company-lsp company-backends))


