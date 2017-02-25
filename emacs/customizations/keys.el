;; Use F2 to eval-and-replace lisp expressions
(global-set-key [f2] 'my-eval-and-replace)

;; User F3 to toggle between project buffers
(global-set-key [f3] 'projectile-project-buffers-other-buffer)

;; Use F4 to search buffers to switch to
(global-set-key [f4] 'switch-to-buffer)

;; Use F7 to clear repl buffer
(global-set-key [f7] 'comint-clear-buffer)

;; Use F9 to kill current buffer
(global-set-key [f9] 'kill-buffer-and-window)
