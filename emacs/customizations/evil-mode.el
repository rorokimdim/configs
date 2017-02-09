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
  "h" 'previous-buffer
  "l" 'next-buffer
  "w" 'save-buffer
  "k" 'kill-buffer
  "*" (lambda ()
        (interactive)
        (mapc 'kill-buffer (buffer-list)))
  "s" 'speedbar
  "d" 'cd
  "t" 'multi-term
  "x" 'smex
  "o" 'other-window
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right)

;; Disable evil for modes where it's worse
(add-to-list 'evil-emacs-state-modes 'calculator-mode)
(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)

;; Start term-mode in emacs mode
(evil-set-initial-state 'term-mode 'emacs)

;; Shortcuts for lisp interaction mode
(evil-leader/set-key-for-mode 'lisp-interaction-mode
    "e" 'eval-defun
    "p"  (lambda ()
           (interactive)
           (forward-char)
           (eval-print-last-sexp)))

;; Shortcuts for emacs-lisp mode
(evil-leader/set-key-for-mode 'emacs-lisp-mode
    "e" 'eval-defun
    "r" 'eval-current-buffer)

;; Shortcuts for python mode
(evil-leader/set-key-for-mode 'python-mode
    "r" (lambda ()
          (interactive)
          (run-python)
          (switch-to-buffer "*Python*")))

;; Shortcuts for clojure mode
(evil-leader/set-key-for-mode 'clojure-mode
  "c" 'cider-jack-in
  "f" 'cider-format-buffer
  "r" (lambda ()
        (interactive)
        (cider-load-buffer)
        (cider-switch-to-repl-buffer))
  "e" 'eval-last-sexp)
