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
  "f" 'ido-find-file
  "s" (lambda ()
        (interactive)
        (call-interactively 'ag)
        (delete-window))
  "t" (lambda ()
        (interactive)
        (call-interactively 'term)
        (call-interactively 'end-of-buffer))
  "x" 'er/expand-region
  "q" 'kill-buffer-and-window
  "o" 'ace-window
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right)

;; Disable evil for modes where it's worse
(cl-loop for mode in '(calculator-mode
                       cider-docview-mode
                       cider-inspector-mode
                       cider-macroexpansion-mode
                       cider-stacktrace-mode
                       cider-test-report-mode
                       cider-repl-mode
                       geiser-doc-mode
                       term-mode
                       image-mode
                       custom-theme-choose-mode)
         do (evil-set-initial-state mode 'emacs))

;; Shortcuts for restclient mode
(evil-leader/set-key-for-mode 'restclient-mode
  "er" 'restclient-http-send-current-raw
  "ef" 'restclient-http-send-current
  "ec" 'restclient-copy-curl-command
  "rn" 'restclient-jump-next
  "rp" 'restclient-jump-prev)

;; Fix shortcuts for neotree
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-dir)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)

;; Use Ctrl-e to go to end of line, like in emacs
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)

;; Use evil-magit
(require 'evil-magit)

;; Treat emacs symbol as a word to facilitate symbol searching
(with-eval-after-load 'evil (defalias #'forward-evil-word #'forward-evil-symbol))

;; Enable visualstar mode
(global-evil-visualstar-mode)

;; Enable evil-anzu mode
(with-eval-after-load 'evil (require 'evil-anzu))
