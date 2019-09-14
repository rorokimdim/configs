(require 'smartparens)

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
  (add-hook h 'my-add-pretty-symbols)
  (add-hook h 'paxedit-mode)
  (add-hook h #'my-add-lispy-shortcuts))

;;
;; Shortcuts
;;
(defun my-add-lispy-shortcuts ()
  "Add shortcuts to a lispy-mode hook."
  (progn
    (evil-local-set-key 'normal (kbd "D") 'sp-kill-sexp)
    (evil-local-set-key 'normal (kbd "<") 'sp-backward-slurp-sexp)
    (evil-local-set-key 'normal (kbd ">") 'sp-forward-slurp-sexp)
    (evil-local-set-key 'normal (kbd "M-r") 'sp-raise-sexp)
    (evil-local-set-key 'normal (kbd "M-<") 'sp-backward-barf-sexp)
    (evil-local-set-key 'normal (kbd "M->") 'sp-forward-barf-sexp)))

(require 'bind-map)
(bind-map my-lisp-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (clojure-mode
                clojurescript-mode
                emacs-lisp-mode
                ielm-mode-hook
                lisp-mode
                scheme-mode)
  :bindings ("(" 'sp-wrap-round
             ")" 'sp-wrap-round
             "[" 'sp-wrap-square
             "]" 'sp-wrap-square
             "{" 'sp-wrap-curly
             "}" 'sp-wrap-curly
             "d" 'sp-kill-sexp
             "y" 'sp-copy-sexp))

;; Prevent smarparens from autoclosing single quotes
(sp-with-modes sp-lisp-modes
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" nil :actions nil))

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
