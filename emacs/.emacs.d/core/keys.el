;;
;; Common global keyboard shortcuts
;;
;; Put mode specific shortcuts elsewhere.
;;

;; Change text scale
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; git-grep
(global-set-key (kbd "C-c g") 'helm-git-grep-at-point)

;; avy
(global-set-key (kbd "C-c ,") 'avy-goto-char)
(global-set-key (kbd "C-c f") 'avy-goto-char-in-line)

;; Show major mode commands
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Use super-<arrow-key> to switch between windows
(global-set-key (kbd "s-<left>") 'evil-window-left)
(global-set-key (kbd "s-<right>") 'evil-window-right)
(global-set-key (kbd "s-<up>") 'evil-window-up)
(global-set-key (kbd "s-<down>") 'evil-window-down)

;; Shortcuts for frame size
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-s-<268632077>") 'toggle-frame-maximized)  ;; ctrl + super + m

;; Use F2 to eval-and-replace lisp expressions
(global-set-key [f2] 'my/eval-and-replace)

;; Use F4/super-r to change window layout
(global-set-key [f4] 'rotate-layout)
(global-set-key (kbd "C-c C-w w") 'rotate-window)
(global-set-key (kbd "C-c C-w r") 'rotate-layout)
(global-set-key (kbd "s-r") 'rotate-layout)

(global-set-key (kbd "C-c C-n") 'next-buffer)
(global-set-key (kbd "C-c C-p") 'previous-buffer)

;; Use F10 to clear repl buffer
(global-set-key [f10] 'comint-clear-buffer)

;;
;; bind-maps
;; See https://github.com/justbur/emacs-bind-map
;;

;; For file and buffer stuff
(require 'bind-map)
(require 'emamux)
(bind-map my/back-slash-leader-map
  :keys ("s-\\")
  :evil-keys ("\\")
  :evil-states (normal visual)
  :bindings ("\\" 'counsel-switch-buffer
             "|" 'my/split-window-horizontally
             "-" 'my/split-window-vertically
             "*" 'my/kill-all-other-buffers
             "b" 'my/neotree-projectile-toggle
             "c" 'my/tmux-open-emacs-config
             "e" 'my/tmux-new-editor
             "f" 'find-file-in-project
             "gb" 'magit-blame
             "gg" 'helm-git-grep-at-point
             "gs" 'magit-status
             "m" 'helm-imenu
             "n" 'my/tmux-open-notes
             "q" 'evil-quit-all
             "r" 'helm-recentf
             "s" 'my/search-with-ag
             "pf" 'counsel-projectile-find-file
             "po" 'counsel-projectile-switch-project
             "pc" 'projectile-kill-buffers
             "ts" 'emamux:send-command
             "tr" 'emamux:run-command
             "tl" 'emamux:run-last-command
             "tc" 'emamux:close-panes
             "x" 'kill-buffer-and-window))

;; For "programs"
(bind-map my/back-quote-leader-map
  :keys ("s-`")
  :evil-keys ("`")
  :evil-states (normal visual)
  :bindings ("`" 'my/browse-project-directory
             "b" 'helm-bookmarks
             "s" 'eshell
             "d" 'osx-dictionary-search-input
             "g" 'helm-google-suggest
             "c" 'calc
             "j" 'calendar
             "t" 'world-time-list))

;; For "shells" and windows and tabs
(bind-map my/space-leader-map
  :keys ("s-SPC")
  :evil-keys ("SPC")
  :evil-states (normal visual)
  :bindings ("'" 'my/tmux-open-shell-in-project-directory
             "\"" 'my/tmux-open-shell-in-buffer-directory
             "c" 'my/tmux-cd-configs
             "d" 'my/tmux-cd-workspace
             "r" 'rotate-window
             "l" 'rotate-layout
             "o" 'ace-window
             "p" 'my/tmux-play-cmd
             "s" 'ace-swap-window
             "tc" 'tab-close
             "to" 'tab-new
             "tm" 'tab-move
             "ts" 'tab-switch
             "0" 'delete-window
             "1" 'delete-other-windows))

;; For windows
(bind-map my/f3-leader-map
  :keys ("<f3>")
  :bindings ("r" 'rotate-window
             "l" 'rotate-layout
             "s" 'ace-swap-window
             "o" 'ace-window
             "0" 'delete-window
             "1" 'delete-other-windows))

;; For anything that needs to be used often
(bind-map my/comma-leader-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :bindings ("<SPC>" 'er/expand-region
             "," 'avy-goto-char

             "do" 'xref-find-definitions-other-window
             "dj" 'xref-find-definitions
             "db" 'xref-pop-marker-stack
             "di" 'dumb-jump-go-prompt

             "f" 'avy-goto-char-in-line

             "w0" 'delete-window
             "w1" 'delete-other-windows
             "wh" 'evil-window-left
             "wl" 'evil-window-right
             "wk" 'evil-window-up
             "wj" 'evil-window-down
             "o" 'ace-window

             "gs" 'find-function
             "gd" 'evil-goto-definition

             "eb" 'eval-buffer
             "ee" 'eval-last-sexp
             "ef" 'eval-defun
             "ep" 'my/eval-print-last-sexp
             "er" 'eval-region
             "ex" 'my/eval-and-replace

             "hd" 'describe-function

             "ma" 'evil-multiedit-match-all
             "mn" 'evil-multiedit-match-and-next
             "mp" 'evil-multiedit-match-and-prev
             "msn" 'evil-multiedit-match-symbol-and-next
             "msp" 'evil-multiedit-match-symbol-and-prev

             "to" 'tab-new
             "tc" 'tab-close
             "tm" 'tab-move
             "ts" 'tab-switch

             "/" 'helm-swoop

             "-" 'my/split-window-vertically
             "|" 'my/split-window-horizontally))
