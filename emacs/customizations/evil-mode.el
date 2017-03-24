(require 'evil)

;; Enable evil mode by default
;; To toggle evil mode use Ctrl-z
(evil-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; Shortcuts that should work on both insert and normal mode
(cl-loop for (key f) in '(("M-0" sp-forward-slurp-sexp)
                          ("M-9" sp-backward-slurp-sexp)
                          ("M-a" evil-beginning-of-line)
                          ("M-e" evil-end-of-line)
                          ("C-a" evil-beginning-of-line)
                          ("C-e" evil-end-of-line)
                          ("M-n" evil-next-line)
                          ("M-p" evil-previous-line))
         do (define-key evil-normal-state-map (kbd key) f)
            (define-key evil-insert-state-map (kbd key) f))

;; General shortcuts
(evil-leader/set-key
  "<SPC>" 'eval-expression
  "," 'ace-jump-mode
  "wc" 'eyebrowse-create-window-config
  "wd" (lambda ()
         (interactive)
         (call-interactively 'eyebrowse-create-window-config)
         (call-interactively 'ido-dired))
  "wf" (lambda ()
         (interactive)
         (call-interactively 'eyebrowse-create-window-config)
         (call-interactively 'my-find-file))
  "wt" (lambda ()
         (interactive)
         (call-interactively 'eyebrowse-create-window-config)
         (call-interactively 'helm-mt))
  "w." 'eyebrowse-switch-to-window-config
  "w," 'eyebrowse-rename-window-config
  "w'" 'eyebrowse-next-window-config
  "w\"" 'eyebrowse-close-window-config
  "wn" 'eyebrowse-next-window-config
  "wp" 'eyebrowse-prev-window-config
  "w0" 'eyebrowse-switch-to-window-config-0
  "w1" 'eyebrowse-switch-to-window-config-1
  "w2" 'eyebrowse-switch-to-window-config-2
  "w3" 'eyebrowse-switch-to-window-config-3
  "w4" 'eyebrowse-switch-to-window-config-4
  "w5" 'eyebrowse-switch-to-window-config-5
  "w6" 'eyebrowse-switch-to-window-config-6
  "w7" 'eyebrowse-switch-to-window-config-7
  "w8" 'eyebrowse-switch-to-window-config-8
  "w9" 'eyebrowse-switch-to-window-config-9
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  "ww" 'rotate-layout
  "wr" 'rotate-window
  "ws" 'ace-swap-window
  "gs" 'find-function
  "gd" 'evil-goto-definition
  "eb" 'eval-buffer
  "ee" 'eval-last-sexp
  "ef" 'eval-defun
  "ep" 'my-eval-print-last-sexp
  "er" 'eval-region
  "ex" 'my-eval-and-replace
  "b" 'switch-to-buffer
  "*" (lambda ()
        (interactive)
        (mapc 'kill-buffer (buffer-list)))
  "h" (lambda ()
        (interactive)
        (my-buffer-toggle 'previous-buffer))
  "l" (lambda ()
        (interactive)
        (my-buffer-toggle 'next-buffer))
  "m" 'mc/mark-all-like-this
  "n" 'neotree-toggle
  "d" 'ido-dired
  "f" 'my-find-file
  "s" (lambda ()
        (interactive)
        (call-interactively 'ag)
        (delete-window))
  "t" 'helm-mt
  "x" 'er/expand-region
  "q" 'kill-buffer-and-window
  "o" 'ace-window
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'helm-google-suggest
  "4" 'helm-recentf
  "6" 'eshell

  "50" 'delete-frame
  "5o" 'other-frame
  "5c" (lambda ()
         (interactive)
         (call-interactively 'make-frame)
         (switch-to-buffer "*scratch*"))
  "5d" (lambda ()
         (interactive)
         (call-interactively 'make-frame)
         (switch-to-buffer "*scratch*")
         (call-interactively 'ido-dired))
  "5f" (lambda ()
         (interactive)
         (call-interactively 'make-frame)
         (switch-to-buffer "*scratch*")
         (call-interactively 'my-find-file))
  "5t" (lambda ()
         (interactive)
         (call-interactively 'make-frame)
         (switch-to-buffer "*scratch*")
         (call-interactively 'helm-mt))

  "\\" (lambda ()
         (interactive)
         (call-interactively 'split-window-right)
         (other-window 1 nil)
         (switch-to-buffer "*scratch*")
         (call-interactively 'my-find-file))
  "=" (lambda ()
        (interactive)
        (call-interactively 'split-window-below)
        (other-window 1 nil)
        (switch-to-buffer "*scratch*")
        (call-interactively 'my-find-file))
  "-" (lambda ()
        (interactive)
        (call-interactively 'split-window-below)
        (other-window 1 nil)
        (switch-to-buffer "*scratch*"))
  "|" (lambda ()
        (interactive)
        (call-interactively 'split-window-right)
        (other-window 1 nil)
        (switch-to-buffer "*scratch*")))

;; Disable evil for modes where it's worse
(cl-loop for mode in '(calculator-mode
                       eshell-mode
                       term-mode
                       image-mode
                       custom-theme-choose-mode)
         do (evil-set-initial-state mode 'emacs))

;; Use Ctrl-e to go to end of line, like in emacs
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)

;; More insert mode shortcuts
(define-key evil-insert-state-map "\C-x\C-n" 'evil-complete-next-line)
(define-key evil-insert-state-map "\C-x\C-p" 'evil-complete-previous-line)

;; Use evil-magit
(require 'evil-magit)

;; Treat emacs symbol as a word to facilitate symbol searching
(with-eval-after-load 'evil (defalias #'forward-evil-word #'forward-evil-symbol))

;; Enable visualstar mode
(global-evil-visualstar-mode)

;; Enable evil-anzu mode
(with-eval-after-load 'evil (require 'evil-anzu))

;; Prevent emacs from quitting on :q or :wq
(defun my-evil-quit (old-fun &rest args)
  (call-interactively 'kill-buffer-and-window))
(advice-add #'evil-quit :around #'my-evil-quit)
