(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ensure t
  :config

  ;; disable file-watchers to improve performance
  (setq lsp-enable-file-watchers nil)

  ;; doc popups are annyoying most of the time
  (setq lsp-ui-doc-enable nil)

  ;; flycheck doesn't seem to work (max depth exceeded error)
  (setq lsp-diagnostic-package :none)
  (setq lsp-flycheck-live-reporting nil)

  ;; symbol highlighting has too much visual distraction
  (setq lsp-enable-symbol-highlighting nil)

  ;; sidelines are annoying most of the time
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-diagnostics nil))

(use-package lsp-ui :commands lsp-ui-mode :ensure t)

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(defun my-lsp-ui-doc-toggle ()
  "Toggles lsp-ui-doc."
  (interactive)
  (if (lsp-ui-doc--frame-visible-p)
      (lsp-ui-doc-hide)
    (lsp-ui-doc-glance)))
