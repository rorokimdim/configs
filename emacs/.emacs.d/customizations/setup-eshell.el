(add-hook
 'eshell-mode-hook
 (lambda ()
   (require 'eshell-autojump)
   (require 'eshell-up)
   (require 's)
   (require 'f)
   (remove-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)

   (local-set-key (kbd "C-l")  (lambda ()  (interactive)  (recenter 0)))
   (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)

   (setq-local global-hl-line-mode nil)
   (setq pcomplete-cycle-completions nil)
   (setq eshell-visual-subcommands '(("git" "commit" "l" "log" "lol" "mine" "show")))
   (setq eshell-visual-commands '("less" "htop" "top" "vim" "ipython" "tmux" "psql"))))
