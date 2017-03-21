;; Disable hl-line mode for eshell
(add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
