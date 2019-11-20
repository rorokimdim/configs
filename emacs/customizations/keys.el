;;
;; Common global keyboard shortcuts
;;
;; Put mode specific shortcuts elsewhere.
;;

;; Change text scale
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Use super-n to switch to nth eyebrowse window
(global-set-key (kbd "s-0") 'eyebrowse-switch-to-window-config-0)
(global-set-key (kbd "s-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "s-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "s-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "s-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "s-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "s-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "s-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "s-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "s-9") 'eyebrowse-switch-to-window-config-9)
(global-set-key (kbd "s-,") 'eyebrowse-prev-window-config)
(global-set-key (kbd "s-.") 'eyebrowse-next-window-config)


;; Use super-<arrow-key> to switch between windows
(global-set-key (kbd "s-<left>") 'evil-window-left)
(global-set-key (kbd "s-<right>") 'evil-window-right)
(global-set-key (kbd "s-<up>") 'evil-window-up)
(global-set-key (kbd "s-<down>") 'evil-window-down)

;; Shortcuts for frame size
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-s-<268632077>") 'toggle-frame-maximized)  ;; ctrl + super + m

;; Use F2 to eval-and-replace lisp expressions
(global-set-key [f2] 'my-eval-and-replace)

;; Use F4/super-r to change window layout
(global-set-key [f4] 'rotate-layout)
(global-set-key (kbd "C-c C-w w") 'rotate-window)
(global-set-key (kbd "C-c C-w r") 'rotate-layout)
(global-set-key (kbd "s-r") 'rotate-layout)

;; Use F10 to clear repl buffer
(global-set-key [f10] 'comint-clear-buffer)

;; Do not ask for keyword when using ffip-split-window-*
(setq ffip-split-window-without-asking-for-keyword t)

;;
;; bind-maps
;; See https://github.com/justbur/emacs-bind-map
;;

;; For file and buffer stuff
(require 'bind-map)
(require 'emamux)
(bind-map my-back-slash-leader-map
  :keys ("s-\\")
  :evil-keys ("\\")
  :evil-states (normal visual)
  :bindings ("\\" 'ido-switch-buffer
             "|" 'ffip-split-window-horizontally
             "-" 'ffip-split-window-vertically
             "*" (lambda ()
                   (interactive)
                   (mapc 'kill-buffer (delq "*scratch*" (buffer-list))))
             "c" 'my-find-config-file
             "d" (lambda ()
                   (interactive)
                   (emamux:tmux-run-command
                    nil
                    "new-window"
                    "cd ~/workspace/$(ls -d ~/workspace/*/ | sed \"s/.*workspace//\" | sed \"s:/::g\" | fzf); exec bash"))
             "f" 'find-file-in-project
             "m" 'helm-imenu
             "q" 'evil-quit-all
             "e" (lambda ()
                   (interactive)
                   (emamux:tmux-run-command nil "new-window" "emacsclient" "-t"))
             "r" 'helm-recentf
             "s" (lambda ()
                   (interactive)
                   (call-interactively 'ag)
                   (delete-window))
             "ts" 'emamux:send-command
             "tr" 'emamux:run-command
             "tl" 'emamux:run-last-command
             "tc" 'emamux:close-panes
             "0" 'eyebrowse-switch-to-window-config-0
             "1" 'eyebrowse-switch-to-window-config-1
             "2" 'eyebrowse-switch-to-window-config-2
             "3" 'eyebrowse-switch-to-window-config-3
             "4" 'eyebrowse-switch-to-window-config-4
             "5" 'eyebrowse-switch-to-window-config-5
             "6" 'eyebrowse-switch-to-window-config-6
             "7" 'eyebrowse-switch-to-window-config-7
             "8" 'eyebrowse-switch-to-window-config-8
             "9" 'eyebrowse-switch-to-window-config-9
             "x" 'kill-buffer-and-window))

;; For "programs"
(bind-map my-back-quote-leader-map
  :keys ("s-`")
  :evil-keys ("`")
  :evil-states (normal visual)
  :bindings ("`" (lambda ()
                   (interactive)
                   (deer (ffip-get-project-root-directory)))
             "s" 'eshell
             "d" 'osx-dictionary-search-input
             "g" 'helm-google-suggest
             "c" 'calc
             "j" 'calendar
             "t" 'world-time-list))

;; For windows
(bind-map my-f3-leader-map
  :keys ("<f3>")
  :bindings ("c" 'eyebrowse-next-window-config
             "x" 'eyebrowse-close-window-config
             "r" 'rotate-window
             "l" 'rotate-layout
             "s" 'ace-swap-window
             "," 'eyebrowse-rename-window-config
             "o" 'ace-window
             "0" 'delete-window
             "1" 'delete-other-windows))

;; For anything that needs to be used often
(bind-map my-comma-leader-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :bindings ("<SPC>" 'er/expand-region
             "," 'avy-goto-char

             "do" 'dumb-jump-go-other-window
             "dj" 'dumb-jump-go
             "db" 'dumb-jump-back
             "di" 'dumb-jump-go-prompt

             "f" 'avy-goto-char-in-line

             "w0" 'delete-window
             "w1" 'delete-other-windows
             "wh" 'evil-window-left
             "wl" 'evil-window-right
             "wk" 'evil-window-up
             "wj" 'evil-window-down
             "o" 'ace-window

             "gb" 'magit-blame

             "gs" 'find-function
             "gd" 'evil-goto-definition

             "eb" 'eval-buffer
             "ee" 'eval-last-sexp
             "ef" 'eval-defun
             "ep" 'my-eval-print-last-sexp
             "er" 'eval-region
             "ex" 'my-eval-and-replace

             "m" 'mc/mark-more-like-this-extended
             "/" 'helm-swoop

             "s|" 'ffip-split-window-horizontally
             "s-" 'ffip-split-window-vertically

             "-" (lambda ()
                   (interactive)
                   (call-interactively 'split-window-below)
                   (other-window 1 nil)
                   (switch-to-buffer "*scratch*"))
             "|" (lambda ()
                   (interactive)
                   (call-interactively 'split-window-right)
                   (other-window 1 nil)
                   (switch-to-buffer "*scratch*"))))

;;
;; Key chords
;;

(require 'key-chord)
(key-chord-define-global "~~" 'helm-bookmarks)
(key-chord-mode 1)

;; Disable key chord in minibuffer
(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (set (make-local-variable 'input-method-function) nil)))
