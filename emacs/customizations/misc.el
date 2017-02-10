;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Use restclient mode for .rest files
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Get rid of messsage buffer. I seldom need it.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")


