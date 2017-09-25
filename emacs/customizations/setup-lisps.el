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
  (add-hook h 'paxedit-mode))

;; Prevent smarparens from autoclosing single quotes
(require 'smartparens)
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

;;
;; Key bindings for smartparens/paxedit
;;
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
    "j" 'sp-end-of-sexp
    "y" 'paxedit-copy
    "^" 'paxedit-sexp-raise))

(evil-define-key 'normal paxedit-mode-map
  ">e" 'paxedit-transpose-forward
  "<e" 'paxedit-transpose-backward
  "<I" (lambda ()
         (interactive)
         (paxedit-backward-up 1)
         (evil-append 1))
  ">I" (lambda ()
         (interactive)
         (paxedit-backward-end 1)
         (backward-char)
         (evil-insert 1))
  "<<" 'paxedit-previous-symbol
  ">>" 'paxedit-next-symbol
  ">k" 'paxedit-forward-kill
  "<k" 'paxedit-backward-kill
  ">)" 'sp-forward-barf-sexp
  "<(" 'sp-backward-barf-sexp)

(eval-after-load "paxedit"
  '(progn
     (define-key paxedit-mode-map (kbd "C-&") 'paxedit-kill)
     (define-key paxedit-mode-map (kbd "C-*") 'paxedit-delete)
     ;; Symbol manipulation
     (define-key paxedit-mode-map (kbd "M-u") 'paxedit-symbol-change-case)
     (define-key paxedit-mode-map (kbd "C-@") 'paxedit-symbol-copy)
     (define-key paxedit-mode-map (kbd "C-#") 'paxedit-symbol-kill)))
