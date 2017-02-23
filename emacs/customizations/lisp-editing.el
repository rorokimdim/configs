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
  (add-hook h 'turn-on-eldoc-mode)
  (add-hook h 'highlight-sexp-mode)
  (add-hook h 'prettify-symbols-mode)
  (add-hook h 'my-add-pretty-symbols))

;; Setup geiser
(setq geiser-active-implementations '(racket))

;; Get rid of highlight color in smartparens
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

;; Prevent smarparens from autoclosing single quotes
(require 'smartparens)
(sp-with-modes sp--lisp-modes
  (sp-local-pair "'" nil :actions nil))
