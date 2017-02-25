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
  "gd" 'evil-goto-definition
  "eb" 'eval-buffer
  "ee" 'eval-last-sexp
  "ef" 'eval-defun
  "ep" 'my-eval-print-last-sexp
  "er" 'eval-region
  "ex" 'my-eval-and-replace
  "b" 'switch-to-buffer
  "w" 'save-buffer
  "*" 'projectile-kill-buffers
  "m" 'mc/mark-all-like-this
  "n" 'neotree-toggle
  "d" 'neotree-dir
  "f" 'ido-find-file
  "s" 'ag
  "pb" 'projectile-switch-to-buffer
  "pd" (lambda ()
        (interactive)
        (let ((ndir (read-directory-name "Enter directory: ")))
          (cd ndir)
          (projectile-switch-project-by-name ndir)))
  "pf" (lambda ()
        (interactive)
        (call-interactively #'projectile-ag)
        (delete-window))
  "pp" 'projectile-switch-project
  "a" 'ace-jump-mode
  "t" 'multi-term
  "x" 'er/expand-region
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
                       cider-stacktrace-mode
                       cider-test-report-mode
                       cider-repl-mode
                       geiser-doc-mode
                       term-mode
                       custom-theme-choose-mode)
         do (evil-set-initial-state mode 'emacs))

;; Shortcuts for lisp modes
(dolist (m '(lisp-interaction-mode
             emacs-lisp-mode
             scheme-mode
             clojure-mode))
  (evil-leader/set-key-for-mode m
    ")" 'sp-forward-slurp-sexp
    "(" 'sp-backward-slurp-sexp
    "h" 'sp-previous-sexp
    "l" 'sp-next-sexp
    "j" 'sp-down-sexp
    "k" 'sp-up-sexp))

;; Shortcuts for clojure mode
(evil-leader/set-key-for-mode 'clojure-mode
  "cc" 'cider-connect
  "cj" 'cider-jack-in
  "cd" 'cider-doc
  "cgd" 'cider-grimoire-web
  "cf" 'cider-format-buffer
  "r" 'cider-load-buffer-and-switch-to-repl-buffer
  "eb" 'cider-load-buffer
  "ee" 'cider-eval-last-sexp
  "ef" 'cider-eval-defun-at-point
  "er" 'cider-eval-region
  "ex" 'cider-eval-last-sexp-and-replace)

;; Shortcuts for scheme mode
(evil-leader/set-key-for-mode 'scheme-mode
  "r" (lambda ()
        (interactive)
        (geiser-mode-switch-to-repl-and-enter)
        (delete-other-windows)
        (geiser-repl-clear-buffer))
  "eb" 'geiser-eval-buffer
  "ee" 'geiser-eval-last-sexp
  "ef" 'geiser-eval-definition
  "er" 'geiser-eval-region)

;; Shortcuts for python mode
(evil-leader/set-key-for-mode 'python-mode
  "r" (lambda ()
        (interactive)
        (run-python)
        (switch-to-buffer "*Python*"))
  "gd" 'elpy-goto-definition
  "pd" 'elpy-doc)

;; Shortcuts for restclient mode
(evil-leader/set-key-for-mode 'restclient-mode
  "er" 'restclient-http-send-current-raw
  "ef" 'restclient-http-send-current
  "ec" 'restclient-copy-curl-command
  "rn" 'restclient-jump-next
  "rp" 'restclient-jump-prev
  )

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
