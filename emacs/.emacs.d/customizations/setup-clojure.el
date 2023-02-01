;;;;
;; Clojure
;;;;

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.cljc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.closhrc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("closhrc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))

;; clj-refactor
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (setq clojure-use-backtracking-indent nil)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Shortcuts for clojure mode
(require 'bind-map)
(bind-map my/clj-cljs-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (clojure-mode
                clojurescript-mode)
  :bindings ("cj" 'cider-jack-in
             "cc" 'cider-connect
             "r" 'cljr-rename-symbol
             "eb" 'cider-eval-buffer
             "ee" 'cider-eval-last-sexp
             "er" 'cider-eval-region
             "ef" 'cider-eval-defun-at-point
             "hd" 'cider-clojuredocs
             "hh" 'cider-apropos-select
             "hw" 'cider-clojuredocs-web
             "nr" 'cider-ns-refresh))

(with-eval-after-load 'cider-repl
  (define-key cider-repl-mode-map (kbd "C-l") 'cider-repl-clear-buffer)
  (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input))

(setq
 cider-repl-display-help-banner nil
 cider-repl-pop-to-buffer-on-connect nil
 cider-repl-display-in-current-window t
 cider-repl-buffer-size-limit 5000
 cider-print-buffer-size nil
 cider-show-error-buffer nil
 clojure-toplevel-inside-comment-form t
 cider-dynamic-indentation nil
 cider-font-lock-dynamically nil
 cider-font-lock-reader-conditionals nil)

(defun my/cider-on-disconnect ()
  (message "[cider] repl disconnected; killing repl buffer *cider-repl.")
  (cl-loop for buffer in (buffer-list)
           do (if (string-prefix-p "*cider-repl" (buffer-name buffer))
                  (kill-buffer buffer))))

(add-hook 'cider-disconnected-hook #'my/cider-on-disconnect)
