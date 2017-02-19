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
  "gs" 'find-function
  "gd" 'describe-function
  "eb" 'eval-buffer
  "ee" 'eval-last-sexp
  "ef" 'eval-defun
  "ep" 'my-eval-print-last-sexp
  "er" 'eval-region
  "ex" 'my-eval-and-replace
  "b" 'switch-to-buffer
  "h" (lambda () (interactive) (my-buffer-toggle 'previous-buffer))
  "l" (lambda () (interactive) (my-buffer-toggle 'next-buffer))
  "w" 'save-buffer
  "k" 'kill-buffer
  "*" (lambda ()
        (interactive)
        (mapc 'kill-buffer (buffer-list)))
  "n" 'neotree-toggle
  "d" 'neotree-dir
  "f" 'ag
  "pp" 'projectile-switch-project
  "pd" (lambda ()
        (interactive)
        (let ((ndir (read-directory-name "Enter directory: ")))
          (cd ndir)
          (projectile-switch-project-by-name ndir)))
  "pf" (lambda ()
        (interactive)
        (call-interactively #'projectile-ag)
        (delete-window))
  "t" 'multi-term
  "x" 'smex
  "o" 'other-window
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right)

;; Disable evil for modes where it's worse
(cl-loop for mode in '(
                       calculator-mode
                       cider-docview-mode
                       cider-inspector-mode
                       cider-macroexpansion-mode
                       cider-repl-mode
                       cider-stacktrace-mode
                       cider-test-report-mode
                       cider-repl-mode
                       racket-repl-mode
                       term-mode
                       )
         do (evil-set-initial-state mode 'emacs))

;; Shortcuts for lisp modes
(dolist (m '(lisp-interaction-mode
             emacs-lisp-mode
             racket-mode
             scheme-mode
             clojure-mode))
  (evil-leader/set-key-for-mode m
    ")" 'sp-forward-slurp-sexp
    "(" 'sp-backward-slurp-sexp
    "s0" 'sp-forward-slurp-sexp
    "s9" 'sp-backward-slurp-sexp
    "sh" 'sp-previous-sexp
    "sl" 'sp-next-sexp
    "sj" 'sp-down-sexp
    "sk" 'sp-up-sexp))

;; Shortcuts for clojure mode
(evil-leader/set-key-for-mode 'clojure-mode
  "cc" 'cider-connect
  "cj" 'cider-jack-in
  "cd" 'cider-doc
  "cgd" 'cider-grimoire-web
  "cf" 'cider-format-buffer
  "cr" 'cider-load-buffer-and-switch-to-repl-buffer
  "eb" 'cider-load-buffer
  "ee" 'cider-eval-last-sexp
  "ef" 'cider-eval-defun-at-point
  "er" 'cider-eval-region
  "ex" 'cider-eval-last-sexp-and-replace)

;; Shortcuts for racket mode
(evil-leader/set-key-for-mode 'racket-mode
  "rr" (lambda ()
         (interactive)
         (racket-run-and-switch-to-repl)
         (delete-other-windows))
  "rd" 'racket-doc
  "er" 'racket-send-region)

;; Shortcuts for python mode
(evil-leader/set-key-for-mode 'python-mode
  "pr" (lambda ()
         (interactive)
         (run-python)
         (switch-to-buffer "*Python*")))

;; Shortcuts for restclient mode
(evil-leader/set-key-for-mode 'restclient-mode
  "e" 'restclient-http-send-current)

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
