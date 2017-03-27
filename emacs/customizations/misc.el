;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Get rid of messsage buffer. I seldom need it.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Follow symlinks to version controlled file without prompting
(setq vc-follow-symlinks t)

;; List of times to show in helm-world-time
(setq display-time-world-list '(("PST8PDT" "San Francisco")
                                ("America/Chicago" "Chicago")
                                ("America/New_York" "New York")
                                ("Asia/Kathmandu" "Asia/Kathmandu")
                                ("UTC" "UTC")))
