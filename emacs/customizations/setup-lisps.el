;; Enable lispy hooks for all lispy modes
(dolist (h '(emacs-lisp-mode-hook
             eval-expression-minibuffer-setup-hook
             ielm-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook
             scheme-mode-hook
             clojure-mode-hook
             geiser-repl-mode-hook
             cider-repl-mode-hook))
  (add-hook h #'smartparens-mode)
  (add-hook h 'rainbow-delimiters-mode)
  (add-hook h 'turn-on-eldoc-mode)
  (add-hook h 'prettify-symbols-mode)
  (add-hook h 'my-add-pretty-symbols))

;; Get rid of highlight color in smartparens
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

;; Prevent smarparens from autoclosing single quotes
(require 'smartparens)
(sp-with-modes sp-lisp-modes
  (sp-local-pair "'" nil :actions nil))

;; Remove useless comment from scratch buffer
(setq initial-scratch-message "")

;; Shortcuts for selected lisp modes
(require 'evil-leader)
(dolist (m '(lisp-interaction-mode
             emacs-lisp-mode
             scheme-mode
             cider-repl-mode
             clojure-mode))
  (evil-leader/set-key-for-mode m
    ")" 'sp-forward-slurp-sexp
    "(" 'sp-backward-slurp-sexp
    "i" 'sp-down-sexp
    "k" 'sp-beginning-of-sexp
    "j" 'sp-end-of-sexp))
