(require 'evil)

;; Enable evil mode by default
;; To toggle evil mode use Ctrl-z
(evil-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; Define evil-escape sequence
(require 'evil-escape)
(setq-default evil-escape-key-sequence "fd")
(setq-default evil-escape-delay 0.2)
(evil-escape-mode)

;; Enable vim like window switching with arrows
(define-key evil-window-map (kbd "<left>") 'evil-window-left)
(define-key evil-window-map (kbd "<right>") 'evil-window-right)
(define-key evil-window-map (kbd "<up>") 'evil-window-up)
(define-key evil-window-map (kbd "<down>") 'evil-window-down)

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

;; Shortcuts for origami
(require 'origami)
(define-key evil-normal-state-map "Z" 'origami-recursively-toggle-node)
(define-key evil-visual-state-map "Z" 'origami-recursively-toggle-node)

;; Shortcuts for evil-numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Disable evil for modes where it's worse
(cl-loop for mode in '(calculator-mode
                       custom-theme-choose-mode
                       eshell-mode
                       image-mode
                       osx-dictionary-mode
                       term-mode
                       world-time-table-mode
                       cider-repl-mode
                       cider-stacktrace-mode
                       haskell-interactive-mode
                       geiser-debug-mode
                       slime-repl-mode)
         do (evil-set-initial-state mode 'emacs))

;; Use Ctrl-e to go to end of line, like in emacs
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)

;; More insert mode shortcuts
(define-key evil-insert-state-map "\C-x\C-n" 'evil-complete-next-line)
(define-key evil-insert-state-map "\C-x\C-p" 'evil-complete-previous-line)
(define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-insert-state-map "\C-l" 'evil-forward-char)
(define-key evil-insert-state-map "\C-h" 'evil-backward-char)

;; Bind confusing macro-record q key to something useful
(define-key evil-normal-state-map "q" 'my-kill-this-buffer-if-unique-else-delete-window)
(define-key evil-normal-state-map "Q" 'evil-quit-all)

;; Remove any search state on ESC key
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)

;; Use evil-magit
(require 'evil-magit)

;; Treat emacs symbol as a word to facilitate symbol searching
(with-eval-after-load 'evil (defalias #'forward-evil-word #'forward-evil-symbol))

;; Enable visualstar mode
(global-evil-visualstar-mode)

;; Enable evil-anzu mode
(with-eval-after-load 'evil (require 'evil-anzu))

;; Allow moving beyond eol
(setq evil-move-beyond-eol t)
