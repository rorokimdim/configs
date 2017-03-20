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
  "t" 'my-open-term
  "x" 'er/expand-region
  "q" 'kill-buffer-and-window
  "o" 'ace-window
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'helm-google-suggest
  "4" 'helm-recentf
  "5" 'find-file-in-repository
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
       (my-buffer-toggle 'next-buffer))
  "|" (lambda ()
       (interactive)
       (call-interactively 'split-window-right)
       (other-window 1 nil)
       (my-buffer-toggle 'next-buffer)))

;; Disable evil for modes where it's worse
(cl-loop for mode in '(calculator-mode
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
