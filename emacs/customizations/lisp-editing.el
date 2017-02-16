;; Enable lispy hooks for all lispy modes
(dolist (h '(emacs-lisp-mode-hook
             eval-expression-minibuffer-setup-hook
             ielm-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook
             scheme-mode-hook
             clojure-mode-hook
             cider-repl-mode-hook))
  (add-hook h #'smartparens-mode)
  (add-hook h 'turn-on-eldoc-mode)
  (add-hook h 'prettify-symbols-mode)
  (add-hook h 'my-add-pretty-symbols))

;; Customize faces for smartparens
(custom-set-faces
  '(sp-pair-overlay-face ((t (:background "gray19"))))
  '(show-paren-match ((t (:background "dark red")))))
