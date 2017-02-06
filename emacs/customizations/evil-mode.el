(require 'evil)

;; Enable evil mode by default
;; To toggle evil mode use Ctrl-z
(evil-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; general shortcuts
(evil-leader/set-key
    "b" 'switch-to-buffer
    "h" 'previous-buffer
    "l" 'next-buffer
    "w" 'save-buffer
    "k" 'kill-buffer
    "d" 'dired)

;; shortcuts for lisp interaction mode
(evil-leader/set-key-for-mode 'lisp-interaction-mode
    "e" 'eval-last-sexp
    "p" 'eval-print-last-sexp)

;; shortcuts for emacs-lisp mode
(evil-leader/set-key-for-mode 'emacs-lisp-mode
    "e" 'eval-last-sexp
    "r" 'eval-current-buffer)

;; shortcuts for python mode
(evil-leader/set-key-for-mode 'python-mode
    "r" (lambda ()
          (interactive)
          (run-python)
          (switch-to-buffer "*Python*")))

;; shortcuts for clojure mode
(evil-leader/set-key-for-mode 'clojure-mode
    "c" 'cider-jack-in
    "f" 'cider-format-buffer
    "r" (lambda ()
          (interactive)
          (cider-load-buffer)
          (cider-switch-to-repl-buffer))
    "e" 'eval-last-sexp
    "d" 'cider-doc)
