;; Kill buffer on exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

;; Default to bash
(defvar my-term-shell "/bin/bash")
(defadvice term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'term)

(add-hook 'term-mode-hook
          '(lambda ()
             ; Disable hl-line mode for terms
             (setq-local global-hl-line-mode nil)))
