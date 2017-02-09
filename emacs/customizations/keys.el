;; M-n and M-p for next/previous matches under cursor
;; M- to replace all matches under cursor
(global-smartscan-mode t)

(global-set-key [f3]
                (lambda ()
                  (interactive)
                  (list-buffers)
                  (delete-other-windows)
                  (switch-to-buffer "*Buffer List*")))
