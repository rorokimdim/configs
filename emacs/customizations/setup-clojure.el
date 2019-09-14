;;;;
;; Clojure
;;;;

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.closhrc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))

;; Shortcuts for clojure mode
(require 'bind-map)
(bind-map my-clj-cljs-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (clojure-mode
                clojurescript-mode)
  :bindings ("cj" 'cider-jack-in
             "cc" 'cider-connect
             "r"  'cider-switch-to-repl-buffer
             "eb" 'cider-eval-buffer
             "ee" (lambda ()
                    (interactive)
                    (forward-char)
                    (cider-eval-sexp-at-point))
             "ef" 'cider-eval-defun-at-point
             "hd" 'cider-doc))



(with-eval-after-load 'cider-repl
  (define-key cider-repl-mode-map (kbd "C-l") 'cider-repl-clear-buffer)
  (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input))
