;;
;; Common global shortcuts for both evil/emacs modes
;;

;; Term
(global-set-key (kbd "C-x t") 'my-open-term)
(global-set-key [f6] 'my-open-term)

;; Find file
(global-set-key (kbd "C-x f") 'my-find-file)
(global-set-key (kbd "C-x C-f") 'my-find-file)

;; Recent files
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "s-r") 'helm-recentf)
(global-set-key [f11] 'helm-recentf)

;; Use F2 to eval-and-replace lisp expressions
(global-set-key [f2] 'my-eval-and-replace)

;; User F3 to toggle buffers
(global-set-key [f3] (lambda()
                       (interactive)
                       (my-buffer-toggle 'next-buffer)))

;; Use F4 to change window layout
(global-set-key [f4] 'rotate-layout)

;; Use F5 to find file in repository
(global-set-key [f5] 'find-file-in-repository)

;; Use F7 to clear repl buffer
(global-set-key [f7] 'comint-clear-buffer)

;; Use F9 to kill current buffer
(global-set-key [f9] 'kill-buffer-and-window)
