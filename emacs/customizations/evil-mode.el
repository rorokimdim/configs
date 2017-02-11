(require 'evil)

;; Enable evil mode by default
;; To toggle evil mode use Ctrl-z
(evil-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; General shortcuts
(evil-leader/set-key
  "b" 'switch-to-buffer
  "f" (lambda ()
        (interactive)
        (call-interactively #'projectile-ag)
        (delete-window))
  "h" 'previous-buffer
  "l" 'next-buffer
  "w" 'save-buffer
  "k" 'kill-buffer
  "*" (lambda ()
        (interactive)
        (mapc 'kill-buffer (buffer-list)))
  "n" 'neotree-toggle
  "d" (lambda ()
        (interactive)
        (let ((ndir (read-directory-name "Enter directory: ")))
          (cd ndir)
          (projectile-switch-project-by-name ndir)))
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
                       term-mode
                       )
         do (evil-set-initial-state mode 'emacs))

;; Shortcuts for lisp interaction mode
(evil-leader/set-key-for-mode 'lisp-interaction-mode
  ">" 'lispyville->
  "<" 'lispyville-<
  "gs" 'lispy-goto-symbol
  "eb" 'eval-current-buffer
  "ee" 'eval-last-sexp
  "ef" 'eval-defun
  "ep" 'my-eval-print-last-sexp
  "er" 'eval-region
  "ex" 'my-eval-and-replace)

;; Shortcuts for emacs-lisp mode
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  ">" 'lispyville->
  "<" 'lispyville-<
  "gs" 'lispy-goto-symbol
  "eb" 'eval-current-buffer
  "ee" 'eval-last-sexp
  "ef" 'eval-defun
  "ep" 'my-eval-print-last-sexp
  "er" 'eval-region
  "ex" 'my-eval-and-replace)

;; Shortcuts for clojure mode
(evil-leader/set-key-for-mode 'clojure-mode
  "cj" 'cider-jack-in
  "cd" 'cider-doc
  "cgd" 'cider-grimoire-web
  "cf" 'cider-format-buffer
  "gs" 'lispy-goto-symbol
  ">" 'lispyville->
  "<" 'lispyville-<
  "r" 'cider-load-buffer-and-switch-to-repl-buffer
  "eb" 'cider-load-buffer
  "ee" 'cider-eval-last-sexp
  "ef" 'cider-eval-defun-at-point
  "er" 'cider-eval-region
  "ex" 'cider-eval-last-sexp-and-replace)

;; Shortcuts for python mode
(evil-leader/set-key-for-mode 'python-mode
  "r" (lambda ()
        (interactive)
        (run-python)
        (switch-to-buffer "*Python*")))

;; Shortcuts for restclient mode
(evil-leader/set-key-for-mode 'restclient-mode
  "e" 'restclient-http-send-current)

;; Fix shortcuts for neotree
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
