;; M-n and M-p for next/previous matches under cursor
;; M- to replace all matches under cursor
(global-smartscan-mode t)

;; Use F3 to show buffer list
(global-set-key [f3]
                (lambda ()
                  (interactive)
                  (list-buffers)
                  (delete-other-windows)
                  (switch-to-buffer "*Buffer List*")))

;; Use F9 to kill all buffers
(global-set-key [f9]
                (lambda ()
                  (interactive)
                  (mapc 'kill-buffer (buffer-list))))

;; Use F2 and F4 to switch buffers
(global-set-key [f2] 'switch-to-buffer)
(global-set-key [f4] 'previous-buffer)
