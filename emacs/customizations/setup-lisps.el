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
  (add-hook h 'show-smartparens-mode)
  (add-hook h 'highlight-parentheses-mode)
  (add-hook h 'rainbow-delimiters-mode)
  (add-hook h 'turn-on-eldoc-mode)
  (add-hook h 'prettify-symbols-mode)
  (add-hook h 'my-add-pretty-symbols))

;; Prevent smarparens from autoclosing single quotes
(require 'smartparens)
(sp-with-modes sp-lisp-modes
  (sp-local-pair "'" nil :actions nil))

;; Get rid of highlight color in smartparens
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)

;; Remove useless comment from scratch buffer
(setq initial-scratch-message "")

;; aggressive-indent-mode hooks
(dolist (h '(emacs-lisp-mode-hook
             clojure-mode-hook
             lisp-interaction-mode-hook
             scheme-mode-hook))
  (add-hook h #'aggressive-indent-mode))

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
    "I" 'sp-backward-up-sexp
    "k" 'sp-beginning-of-sexp
    "j" 'sp-end-of-sexp))
