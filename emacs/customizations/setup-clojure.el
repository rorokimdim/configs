;;;;
;; Clojure
;;;;

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; display repl in current window rather than in a split window
(setq cider-repl-display-in-current-window t)

;; display port in repl buffer name
(setq nrepl-buffer-name-show-port t)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; remove the help banner
(setq cider-repl-display-help-banner nil)

;; when there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; Use clj-refactor
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas-minor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))

;; Start up figwheel on cider-jack-in-clojurescript
(setq cider-cljs-lein-repl
  "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

;; Shortcuts for clojure mode
(require 'evil-leader)
(evil-leader/set-key-for-mode 'clojure-mode
  "cc"  'cider-connect
  "cj"  'cider-jack-in
  "cd"  'cider-doc
  "cgd" 'cider-grimoire-web
  "cf"  'cider-format-buffer
  "r"   'cider-load-buffer-and-switch-to-repl-buffer
  "eb"  'cider-load-buffer
  "ee"  'cider-eval-last-sexp
  "ef"  'cider-eval-defun-at-point
  "er"  'cider-eval-region
  "ex"  'cider-eval-last-sexp-and-replace)
