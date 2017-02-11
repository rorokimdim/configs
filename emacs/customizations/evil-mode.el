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
  "e" 'eval-defun
  ">" 'lispyville->
  "<" 'lispyville-<
  "p"  (lambda ()
         (interactive)
         (forward-char)
         (eval-print-last-sexp)))

;; Shortcuts for emacs-lisp mode
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "e" 'eval-defun
  ">" 'lispyville->
  "<" 'lispyville-<
  "r" 'eval-current-buffer)

;; Shortcuts for clojure mode
(evil-leader/set-key-for-mode 'clojure-mode
  "c" 'cider-jack-in
  ">" 'lispyville->
  "<" 'lispyville-<
  "f" 'cider-format-buffer
  "r" (lambda ()
        (interactive)
        (cider-load-buffer)
        (cider-switch-to-repl-buffer))
  "e" 'eval-last-sexp)

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
