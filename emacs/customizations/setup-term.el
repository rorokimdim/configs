;; Kill buffer on exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))
;; Default to bash
(defvar my-term-shell "/bin/bash")
(defadvice term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'term)
;; Use C-x as command prefix
(add-hook 'term-mode-hook
          '(lambda ()
             (term-set-escape-char ?\C-x)))

;; Disable hl-line mode for terms
(add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
