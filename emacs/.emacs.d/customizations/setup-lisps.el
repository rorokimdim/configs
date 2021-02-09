(require 'smartparens)

;; Enable lispy hooks for all lispy modes
(dolist (h '(emacs-lisp-mode-hook
             eval-expression-minibuffer-setup-hook
             ielm-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook
             scheme-mode-hook
             clojure-mode-hook
             janet-mode-hook
             geiser-repl-mode-hook
             cider-repl-mode-hook))
  (add-hook h #'show-paren-mode)
  (add-hook h #'smartparens-mode)
  (add-hook h 'highlight-parentheses-mode)
  (add-hook h 'turn-on-eldoc-mode)
  (add-hook h 'prettify-symbols-mode)
  (add-hook h 'my/add-pretty-symbols)
  (add-hook h 'paxedit-mode)
  (add-hook h #'my/add-lispy-shortcuts))

;;
;; Shortcuts
;;
(defun my/add-lispy-shortcuts ()
  "Add shortcuts to a lispy-mode hook."
  (progn
    (evil-local-set-key 'normal (kbd "Y") 'sp-copy-sexp)
    (evil-local-set-key 'normal (kbd "D") 'sp-kill-sexp)
    (evil-local-set-key 'normal (kbd "(") 'sp-beginning-of-sexp)
    (evil-local-set-key 'normal (kbd ")") 'sp-end-of-sexp)
    (evil-local-set-key 'normal (kbd "[") 'sp-backward-sexp)
    (evil-local-set-key 'normal (kbd "]") 'sp-forward-sexp)
    (evil-local-set-key 'normal (kbd "<") 'sp-backward-slurp-sexp)
    (evil-local-set-key 'normal (kbd ">") 'sp-forward-slurp-sexp)
    (evil-local-set-key 'normal (kbd "M-r") 'sp-raise-sexp)
    (evil-local-set-key 'normal (kbd "M-<") 'sp-backward-barf-sexp)
    (evil-local-set-key 'normal (kbd "M->") 'sp-forward-barf-sexp)
    (evil-local-set-key 'normal (kbd "C-c C-f") 'aggressive-indent-indent-defun)))

(require 'bind-map)
(bind-map my/lisp-mode-map
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
             "y" 'sp-copy-sexp
             "n" 'sp-next-sexp
             "p" 'sp-previous-sexp))

;; Prevent smarparens from autoclosing single quotes
(sp-with-modes sp-lisp-modes
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" nil :actions nil))

;; Remove useless comment from scratch buffer
(setq initial-scratch-message "")

;; Highlight matching parenthesis
(setq show-paren-style 'parenthesis)

;;
;; Parenthetical coloring
;;
(custom-set-variables
 '(highlight-parentheses-colors
   '("firebrick1" "DarkOliveGreen4" "salmon4" "SkyBlue3" "khaki" "purple" "turquoise4" "green" "plum3")))

(custom-set-faces
 '(sp-show-pair-from-inside nil)
 '(highlight-parentheses-highlight ((t (:weight bold))) t)
 '(show-paren-match ((t (:background "cyan" :foreground "black")))))

;; aggressive-indent-mode hooks
(dolist (h '(emacs-lisp-mode-hook
             clojure-mode-hook
             lisp-interaction-mode-hook
             scheme-mode-hook))
  (add-hook h #'aggressive-indent-mode))
