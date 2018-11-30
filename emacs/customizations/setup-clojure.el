;;;;
;; Clojure
;;;;

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))

;; Shortcuts for clojure mode
(require 'bind-map)
(bind-map my-clj-cljs-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (clojure-mode
                clojurescript-mode)
  :bindings ("cc" 'inf-clojure
             "cr" 'inf-clojure-connect
             "r"  'inf-clojure-switch-to-repl-buffer
             "eb" 'inf-clojure-eval-buffer
             "ee" (lambda ()
                    (interactive)
                    (forward-char)
                    (inf-clojure-eval-last-sexp))
             "ef" 'inf-clojure-eval-defun
             "er" 'inf-clojure-eval-region))

(with-eval-after-load 'inf-clojure
  (define-key inf-clojure-mode-map (kbd "C-l") 'inf-clojure-clear-repl-buffer)
  (define-key inf-clojure-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inf-clojure-mode-map (kbd "<down>") 'comint-next-input))
