;;
;; Common global shortcuts for both evil/emacs modes
;;

;; Change text scale
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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

;; Use F7 and F8 to switch window configs
(global-set-key [f7] 'eyebrowse-prev-window-config)
(global-set-key [f8] 'eyebrowse-next-window-config)

;; Use F10 to clear repl buffer
(global-set-key [f10] 'comint-clear-buffer)

;; Use F9 to kill current buffer
(global-set-key [f9] 'kill-buffer-and-window)

;; Key chords
(require 'key-chord)
(key-chord-define-global "BB" 'ido-switch-buffer)
(key-chord-define-global "EE" 'eval-expression)
(key-chord-define-global "FF" 'my-find-file)
(key-chord-define-global "JJ" 'ace-jump-mode)
(key-chord-define-global "RR" 'helm-recentf)
(key-chord-define-global "SS" 'eshell)
(key-chord-define-global "~~" 'bookmark-jump)
(key-chord-define-global "TT"
                         (lambda ()
                           (interactive)
                           (call-interactively 'helm-mt)
                           (term-send-end)))
(key-chord-define-global "WW" 'eyebrowse-switch-to-window-config)
(key-chord-define-global "XX" 'smex)
(key-chord-define-global "\\\\" 'eyebrowse-last-window-config)
(key-chord-mode 1)

;; Disable key chord in minibuffer
(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (set (make-local-variable 'input-method-function) nil)))
