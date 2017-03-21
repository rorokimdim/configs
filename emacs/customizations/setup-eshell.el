(add-hook
 'eshell-mode-hook
 (lambda ()
   (require 'eshell-autojump)
   (remove-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)
   (setq-local global-hl-line-mode nil)
   (setq pcomplete-cycle-completions nil)
   (setq eshell-visual-subcommands '(("git" "commit" "l" "log" "lol" "mine" "show")))
   (setq eshell-visual-commands '("less" "htop" "top" "vim" "ipython" "tmux"))))
