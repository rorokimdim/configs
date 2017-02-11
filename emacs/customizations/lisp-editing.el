;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Add lipy-mode hooks
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'eval-expression-minibuffer-setup-hook (lambda () (lispy-mode 1)))
(add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))

;; Add lispyville-mode hook to lispy-mode
(add-hook 'lispy-mode-hook #'lispyville-mode)
