;;
;; Common global shortcuts for both evil/emacs modes
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

;; User F3 to toggle buffers
(global-set-key [f3] (lambda()
                       (interactive)
                       (my-buffer-toggle 'next-buffer)))

;; Use F4 to change window layout
(global-set-key [f4] 'rotate-layout)

;; Use F5 to find file in repository
(global-set-key [f5] 'find-file-in-repository)

;; Use F7 to find a config file
(global-set-key [f7] 'my-find-config-file)

;; Use F10 to clear repl buffer
(global-set-key [f10] 'comint-clear-buffer)

;; Use F9 to kill current buffer
(global-set-key [f9] 'kill-buffer-and-window)

;; Key chords
(require 'key-chord)
(key-chord-define-global "BB" 'ido-switch-buffer)
(key-chord-define-global "DD" 'osx-dictionary-search-input)
(key-chord-define-global "EE" 'eval-expression)
(key-chord-define-global "FF" 'my-find-file)
(key-chord-define-global "GG" 'helm-google-suggest)
(key-chord-define-global "JJ" 'ace-jump-mode)
(key-chord-define-global "MM" 'helm-imenu)

(key-chord-define-global "QQ" 'kill-buffer-and-window)
(key-chord-define-global "RR" 'helm-recentf)
(key-chord-define-global "SS" 'eshell)
(key-chord-define-global "~~" 'helm-bookmarks)

(key-chord-define-global "WW" 'eyebrowse-switch-to-window-config)
(key-chord-define-global "XX" 'smex)
(key-chord-define-global "\\\\" 'eyebrowse-last-window-config)
(key-chord-mode 1)

;; Disable key chord in minibuffer
(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (set (make-local-variable 'input-method-function) nil)))
